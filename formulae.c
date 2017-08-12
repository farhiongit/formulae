
/*
 * formulae: a recursive interpretor of formulae (à la Excel)
 * Copyright (C) 2017 L. Farhi (lrspam at sfr.fr)
 */
#define _GNU_SOURCE

// TODO value_free et value = UNDEF
// TODO get rid of mystrdup, mystrndup, getopts, mygetline (use POSIX instead)
// ERROR when
// b=-a
// a="a"

#include <strings.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <float.h>
#include <stdarg.h>
#include <time.h>
#include <gsl_multimin.h>
#include <gsl/gsl_multiroots.h>
#include <dlfcn.h>
#include <intprops.h>

#include "getopts.h"
#include "formulae.h"
#include "dates.h"

#define CHECK_ALLOC(ptr) \
  ASSERT(ptr, "Memory allocation error")

// ASSERT is already defined in ../tools/getopts.h
#if 0
#define ASSERT(cond,msg) \
  do {\
    if (!(cond)) \
    {\
      const char* str = msg ;\
      if (!str || !*str)\
        str = "Fatal error" ;\
      fprintf(stderr, "%s (in function %s at %s:%i)\n",str,__func__,__FILE__,__LINE__);\
      abort() ;\
    }\
  } while(0)
#endif

/**************************************************************
*                          OBJECTS                            *
**************************************************************/

/**********************************************
Type 'value_t' is defined in file 'formulae.h'.
**********************************************/

typedef struct list_of_constants
{
  char *name;
  value_t value;

  struct list_of_constants *next;
} constant_t;

typedef struct list_of_f_0_arg
{
  char *name;
  double (*value) ();           //TODO: change it to value_t

  struct list_of_f_0_arg *next;
} f0arg_t;

typedef struct list_of_f_1_arg
{
  char *name;
  double (*value) (double);     //TODO: change it to value_t

  struct list_of_f_1_arg *next;
} f1arg_t;

typedef struct list_of_f_2_arg
{
  char *name;
  double (*value) (double, double);     //TODO: change it to value_t

  struct list_of_f_2_arg *next;
} f2args_t;

typedef struct list_of_f_3_arg
{
  char *name;
  double (*value) (double, double, double);     //TODO: change it to value_t

  struct list_of_f_3_arg *next;
} f3args_t;

typedef struct list_of_f_n_arg
{
  char *name;
  struct
  {
    int min;
    int max;
  } nbArgs;
    value_t (*value) (int, const value_t * const);

  struct list_of_f_n_arg *next;
} fnargs_t;

// A special unary function to get the value of an expression
static const char *GET_VALUE_FUNCTION = "value";

/*************************************************************/

typedef struct list_of_dependencies
{
  char *variable_name;

  struct list_of_dependencies *next;
} dependency_t;

typedef struct list_of_alerts
{
  value_t min_value, max_value;
  char *message;

  struct list_of_alerts *next;
} alert_t;

struct list_of_engines;

typedef struct list_of_formulae
{
  char *variable_name;
  char *formula;
  dependency_t *dependencies;
  int circular_dependencies;
  alert_t *alerts;
  value_t value;
  value_t min_range;
  value_t max_range;
  char *pos;
  char *previous_pos;
  int notify_change;
  struct list_of_engines *engine;

  struct list_of_formulae *next;
} formula_t;

typedef struct list_of_libraries
{
  void *handler;
  char *filename;

  struct list_of_libraries *next;
} library_t;

typedef struct list_of_engines
{
  char *name;
  formula_t *formulae;
  constant_t *constants;
  f0arg_t *f0args;
  f1arg_t *f1args;
  f2args_t *f2args;
  f3args_t *f3args;
  fnargs_t *fnargs;
  library_t *libraries;
  int automatic_calculation;
  int automatic_notification;
  int quiet;

  struct list_of_engines *next;
} engine_t;

/*************************************************************/

typedef struct formula_msg_handler_list
{
  formula_msg_handler handler;
  struct formula_msg_handler_list *next;
} formula_msg_handler_t;

typedef struct formula_changed_handler_list
{
  formula_changed_handler handler;
  struct formula_changed_handler_list *next;
} formula_changed_handler_t;

/*************************************************************/

typedef struct fragment
{
  char *begin;
  char *end;
} fragment_t;

typedef struct token
{
  enum
  {
    END = '\0',
    IDENTIFIER,
    VALUE,
    PLUS = '+',
    MINUS = '-',
    MULTIPLY = '*',
    DIVIDE = '/',
    EQ = '=',
    LHPAREN = '(',
    RHPAREN = ')',
    LHBRACKET = '[',
    RHBRACKET = ']',
    ARG_SEP = ';',
    LT = '<',
    GT = '>',
    NEQ,
    LE,
    GE,
    NONE
  } type;
  union
  {
    value_t value;              // for a VALUE
    fragment_t id;              // for an IDENTIFIER (fragment of the formula)
  } attribute;
} token_t;

/**************************************************************
*                     GLOBALS CONSTANTS                       *
**************************************************************/
static const value_t ZERO = {.type = INTEGER,.value = {.integer = 0L} };
static const value_t ONE = {.type = INTEGER,.value = {.integer = 1L} };
static const value_t UNDEF = {.type = UNDEFINED,.value = {.number = 0.} };

/**************************************************************
*                     GLOBALS VARIABLES                       *
**************************************************************/
static engine_t *theEngines = 0;
static library_t *theLibraries = 0;

static formula_msg_handler_t *formulaMsgHandlers;
static formula_changed_handler_t *formulaOnChangeHandlers;

/**************************************************************
*                          TOOLS                              *
**************************************************************/
//TODO: use strdup
static char *const
mystrdup (const char *s)
{
  if (!s)
    return 0;

  char *const ret = malloc ((strlen (s) + 1) * sizeof (*ret));

  CHECK_ALLOC (ret);
  char *str;

  for (str = ret; *s; s++, str++)
    *str = *s;
  *str = 0;

  return ret;
}

//TODO: use strndup
static char *
mystrndup (const char *s, size_t n)
{
  if (!s)
    return 0;

  if (n > strlen (s))
    n = strlen (s);

  char *const ret = malloc ((n + 1) * sizeof (*ret));

  CHECK_ALLOC (ret);
  char *str;

  for (str = ret; *s && n; s++, str++, n--)
    *str = *s;
  *str = 0;

  return ret;
}

/*************************************************************/

//TODO: use getline
static char *
mygetline (const FILE * f, char **lineptr)
{
  char ch;
  size_t len = 0;

  *lineptr = (char *) realloc (*lineptr, sizeof (char));
  CHECK_ALLOC (lineptr);
  (*lineptr)[len] = '\0';

  while ((ch = fgetc ((FILE *) f)) != EOF && ch != '\n')
  {
    len++;
    *lineptr = (char *) realloc (*lineptr, (len + 1) * sizeof (char));
    CHECK_ALLOC (lineptr);
    (*lineptr)[len - 1] = ch;
    (*lineptr)[len] = '\0';
  }

  if (ch == EOF && !len)
  {
    free (*lineptr);
    *lineptr = 0;
  }

  return *lineptr;
}

/*************************************************************/

value_t
MAKE_UNDEFINED ()
{
  return UNDEF;
}

value_t
MAKE_NUMBER (double v)
{
  value_t ret;

  ret.type = NUMBER;
  GET_NUMBER (ret) = v;
  return ret;
}

value_t
MAKE_INTEGER (long int v)
{
  value_t ret;

  ret.type = INTEGER;
  GET_INTEGER (ret) = v;
  return ret;
}

value_t
MAKE_DATE_TIME (struct tm v)
{
  value_t ret;

  ret.type = DATE_TIME;
  GET_DATE_TIME (ret) = v;
  return ret;
}

static value_t
__MAKE_STRING (char *v, int allocate)
{
  if (!v)
    return UNDEF;

  value_t ret;

  ret.type = STRING;
  if (allocate)
  {
    GET_STRING (ret) = malloc ((strlen (v) + 1) * sizeof (*(GET_STRING (ret))));
    CHECK_ALLOC (GET_STRING (ret));
    strcpy (GET_STRING (ret), v);
  }
  else
    GET_STRING (ret) = v;

  return ret;
}

value_t
MAKE_STRING (char *v)
{
  return __MAKE_STRING (v, 1);
}

// 'polymorphic' free value
static void
value_free (value_t * v)
{
  if (IS_STRING (*v))
  {
    CHECK_ALLOC (GET_STRING (*v));
    free (GET_STRING (*v));
  }

  *v = UNDEF;
}

static value_t
value_dup (value_t v)
{
  value_t ret = v;

  if (IS_STRING (v))
  {
    GET_STRING (ret) = mystrdup (GET_STRING (v));
    CHECK_ALLOC (GET_STRING (ret));
  }

  return ret;
}

//__attribute__ ((__unused__))
static value_t
value_move (value_t * v)
{
  value_t ret = *v;

  *v = UNDEF;

  return ret;
}

//__attribute__ ((__unused__))
static int
value_cmp (value_t a, value_t b)
{
  ASSERT (a.type == b.type, "Could not compare values of different type.");

  if (IS_NUMBER (a))
    return (GET_NUMBER (a) > GET_NUMBER (b) ? 1 : (GET_NUMBER (a) < GET_NUMBER (b) ? -1 : 0));
  else if (IS_INTEGER (a))
    return (GET_INTEGER (a) > GET_INTEGER (b) ? 1 : (GET_INTEGER (a) < GET_INTEGER (b) ? -1 : 0));
  else if (IS_DATE_TIME (a))
  {
    long int d = tm_diffseconds (GET_DATE_TIME (a), GET_DATE_TIME (b));

    return d > 0 ? -1 : d < 0 ? 1 : 0;
  }
  else if (IS_STRING (a))
    return strcmp (GET_STRING (a), GET_STRING (b));

  return 0;
}

static value_t _f_to_string (int nbArgs, const value_t * const args);

//__attribute__ ((__unused__))
static char *
value_to_string (value_t val)
{
  static value_t ret;

  value_free (&ret);
  value_t str = _f_to_string (1, &val);

  ret = value_move (&str);

  if (IS_STRING (ret))
    return GET_STRING (ret);
  else
    return 0;
}

/**************************************************************
*                 MESSAGE HANDLERS MANAGER                    *
**************************************************************/
void
formula_msg_handler_add (formula_msg_handler handler)
{
  for (formula_msg_handler_t * ptr = formulaMsgHandlers; ptr; ptr = ptr->next)
    if (ptr->handler == handler)        // handler already registered
      return;

  formula_msg_handler_t *const pev = malloc (sizeof (*pev));

  CHECK_ALLOC (pev);
  pev->handler = handler;
  pev->next = 0;

  if (formulaMsgHandlers == 0)
    formulaMsgHandlers = pev;
  else
  {
    formula_msg_handler_t *ptr;

    for (ptr = formulaMsgHandlers; ptr->next != 0; ptr = ptr->next)
      /* nothing */ ;
    ptr->next = pev;
  }
}

void
formula_msg_handler_remove (formula_msg_handler handler)
{
  formula_msg_handler_t *ptr, *next;

  while (formulaMsgHandlers && (!handler || formulaMsgHandlers->handler == handler))
  {
    ptr = formulaMsgHandlers->next;
    free (formulaMsgHandlers);
    formulaMsgHandlers = ptr;
  }

  for (ptr = formulaMsgHandlers; ptr && ptr->next;)
  {
    if (ptr->next->handler == handler)
    {
      next = ptr->next->next;
      free (ptr->next);
      ptr->next = next;
    }
    else
      ptr = ptr->next;
  }
}

// Raises formula msg event (triggers all handlers)
static void
formula_on_message (const char *engine_name,
                    const char *object_name, formula_msg_severity severity, unsigned int depth, const char *fmt, ...)
{
  if (!engine_name || !object_name)
    return;

  va_list ap;

  va_start (ap, fmt);
  int length = vsnprintf (0, 0, fmt, ap);

  if (length < 0)
    return;
  else
    length++;
  va_end (ap);

  char *msg = malloc (length * sizeof (char));

  CHECK_ALLOC (msg);
  va_start (ap, fmt);
  vsnprintf (msg, length, fmt, ap);
  va_end (ap);

  formula_msg_handler_t *ptr = formulaMsgHandlers;

  for (ptr = formulaMsgHandlers; ptr != 0; ptr = ptr->next)
    if (ptr->handler)
      (ptr->handler) (engine_name, object_name, msg, severity, depth);

  free (msg);
}

/**************************************************************
*             ON FORMULA CHANGED HANDLERS MANAGER             *
**************************************************************/
void
formula_changed_handler_add (formula_changed_handler handler)
{
  for (formula_changed_handler_t * ptr = formulaOnChangeHandlers; ptr; ptr = ptr->next)
    if (ptr->handler == handler)        // handler already registered
      return;

  formula_changed_handler_t *const pev = malloc (sizeof (*pev));

  CHECK_ALLOC (pev);
  pev->handler = handler;
  pev->next = 0;

  if (formulaOnChangeHandlers == 0)
    formulaOnChangeHandlers = pev;
  else
  {
    formula_changed_handler_t *ptr;

    for (ptr = formulaOnChangeHandlers; ptr->next != 0; ptr = ptr->next)
      /* nothing */ ;
    ptr->next = pev;
  }
}

void
formula_changed_handler_remove (formula_changed_handler handler)
{
  formula_changed_handler_t *ptr, *next;

  while (formulaOnChangeHandlers && (!handler || formulaOnChangeHandlers->handler == handler))
  {
    ptr = formulaOnChangeHandlers->next;
    free (formulaOnChangeHandlers);
    formulaOnChangeHandlers = ptr;
  }

  for (ptr = formulaOnChangeHandlers; ptr && ptr->next;)
  {
    if (ptr->next->handler == handler)
    {
      next = ptr->next->next;
      free (ptr->next);
      ptr->next = next;
    }
    else
      ptr = ptr->next;
  }
}

// Raises formula changed event (triggers all handlers)
static void
formula_on_change (const char *engine_name, const char *variable_name, value_t value)
{
  if (!engine_name || !variable_name)
    return;

  formula_changed_handler_t *ptr = formulaOnChangeHandlers;

  for (ptr = formulaOnChangeHandlers; ptr != 0; ptr = ptr->next)
    if (ptr->handler)
      (ptr->handler) (engine_name, variable_name, value);
}

/**************************************************************
*                      FORMULAE MANAGER                       *
**************************************************************/

/*
expr->pos is set to 0 if syntax is incorrect.
expr->value is set to UNDEF if elements of formula are undefined or can not be evaluated.
If expr->value is equal to UNDEF, syntax is checked but formula is not evaluated (for optimization purpose).
*/

static void
formula_invalidate (formula_t * const expr)
{
  expr->pos = 0;                // Invalidate token, next token_get will return token NONE
}

static int
formula_isvalid (formula_t * const expr)
{
  return expr->pos != 0;
}

static void formula_changed (const formula_t * const expr);

static void
formula_unset (formula_t * const form)
{
  if (IS_VALUED (form->value))
  {
    value_free (&form->value);
    formula_changed (form);
  }
}

static formula_t *
formula_allocate (const char *variable_name, const char *formula_string)
{
  formula_t *expr = malloc (sizeof (*expr));

  CHECK_ALLOC (expr);

  expr->variable_name = mystrdup (variable_name);
  expr->formula = mystrdup (formula_string);
  expr->dependencies = 0;
  expr->alerts = 0;
  expr->pos = expr->formula;
  expr->value = UNDEF;
  expr->notify_change = 0;
  expr->engine = 0;
  expr->next = 0;
  expr->min_range = UNDEF;
  expr->max_range = UNDEF;
  expr->circular_dependencies = 0;

  return expr;
}

static void
formula_free (formula_t * expr, int reparse)
{
  // argument "reparse" : if reparse, prepare formula for reparsing : free data before update
  // formula_free(expr, 0) can be called to remove formula 'expr' from engine it belongs to.

  if (!expr)
    return;

  formula_unset (expr);

  dependency_t *ndep;

  for (dependency_t * dep = expr->dependencies; dep; dep = ndep)
  {
    ndep = dep->next;
    free (dep->variable_name);
    free (dep);
  }
  expr->dependencies = 0;
  expr->circular_dependencies = 0;

  if (!reparse)
  {
    if (expr->variable_name)
      free (expr->variable_name);
    expr->variable_name = 0;

    if (expr->formula)
      free (expr->formula);
    expr->formula = 0;

    value_free (&expr->min_range);
    value_free (&expr->max_range);

    alert_t *nal;

    for (alert_t * al = expr->alerts; al; al = nal)
    {
      nal = al->next;
      if (al->message)
        free (al->message);
      free (al);
    }

    if (expr->engine && expr->engine->formulae)
    {
      // Remove formula 'expr' from engine
      formula_t *form = expr->engine->formulae; // 1st formula in the list

      if (form == expr)
        expr->engine->formulae = expr->next;
      else
        while (form->next)
          if (form->next == expr)
            form->next = expr->next;
          else
            form = form->next;
    }

    expr->next = 0;
    expr->engine = 0;

    free (expr);
  }                             // if (!reparse)
}

static int formula_parse (formula_t * const expr);

static int
formula_get (formula_t * const form, value_t * const value)
{
#if 0
  if (!IS_VALUED (form->value))
  {
    formula_on_message (form->engine->name, form->formula, MSG_WARNING, 0, "'%s': Not valued yet.", form->formula);
    return 0;
  }
#endif

  // Updates value of formula
  int automatic = form->engine->automatic_calculation;

  form->engine->automatic_calculation = 1;      //TODO: necessary ?
  ASSERT ((formula_parse (form)), 0);
  form->engine->automatic_calculation = automatic;

  if (value)
    *value = form->value;

  return 1;
}

static int
formula_depends_on (formula_t * const form, const char *variable)
{
  if (!variable || !form || !form->engine)
    return 0;

  for (dependency_t * dep = form->dependencies; dep; dep = dep->next)
  {
    if (!strcmp (dep->variable_name, variable))
      return 1;

    formula_t *f;

    for (f = form->engine->formulae; f && (strcmp (f->variable_name, dep->variable_name)); f = f->next)
      /* nothing */ ;

    if (f && (f == form || f->circular_dependencies || formula_depends_on (f, variable)))
      return 1;
  }

  return 0;
}

/***************************************************************************
* When a variable is changed, all variables refering to it are re-computed *
*                                                                          *
* Recursive scheme:                                                        *
*                                                                          *
*       (formula) ----> formula_parse = UNDEF? --(No)--> formula_set       *
*                          ^               |                |              *
*                          |             (Yes)              |              *
*                          |               |                |              *
*                          |               v                |              *
*                      (parents) <---- formula_changed <----'              *
*                                                                          *
***************************************************************************/

static void
formula_changed (const formula_t * const expr)
{
  if (!expr || !expr->engine || !expr->variable_name)
    return;

  //formula_on_message (expr->engine->name, expr->formula, MSG_INFO, 0, "%s: changed.", expr->variable_name);

  if (IS_VALUED (expr->value))
  {
    if (expr->notify_change && !expr->engine->quiet)
      formula_on_change (expr->engine->name, expr->variable_name, expr->value);

    for (alert_t * al = expr->alerts; al; al = al->next)
      if (
           // NUMBER
           (IS_NUMBER (expr->value)
            && (!IS_NUMBER (al->min_value) || GET_NUMBER (al->min_value) <= GET_NUMBER (expr->value))
            && (!IS_NUMBER (al->max_value) || GET_NUMBER (expr->value) <= GET_NUMBER (al->max_value))) ||
           // INTEGER
           (IS_INTEGER (expr->value)
            && (!IS_INTEGER (al->min_value) || GET_INTEGER (al->min_value) <= GET_INTEGER (expr->value))
            && (!IS_INTEGER (al->max_value) || GET_INTEGER (expr->value) <= GET_INTEGER (al->max_value))) ||
           // STRING
           (IS_STRING (expr->value)
            && (!IS_STRING (al->min_value) || strcmp (GET_STRING (al->min_value), GET_STRING (expr->value)) <= 0)
            && (!IS_STRING (al->max_value) || strcmp (GET_STRING (expr->value), GET_STRING (al->max_value)) <= 0)) ||
           // DATE_TIME
           (IS_DATE_TIME (expr->value)
            && (!IS_DATE_TIME (al->min_value)
                || tm_diffseconds (GET_DATE_TIME (al->min_value), GET_DATE_TIME (expr->value)) >= 0)
            && (!IS_DATE_TIME (al->max_value)
                || tm_diffseconds (GET_DATE_TIME (expr->value), GET_DATE_TIME (al->max_value)) >= 0)) ||
           //
           0)
        formula_on_message (expr->engine->name, expr->variable_name, MSG_INFO, 0, al->message);
  }                             // if (IS_VALUED (expr->value))
  else if (expr->notify_change && !expr->engine->quiet) // && !IS_VALUED (expr->value)
    formula_on_change (expr->engine->name, expr->variable_name, UNDEF); // quiet NaN is implementation dependent

  // Recalculate all formulae which depend on expr
  if (IS_VALUED (expr->value))  // TODO: check if not restrictive here
    if (expr->engine->automatic_calculation)
      for (formula_t * form = expr->engine->formulae; form; form = form->next)
        if (form != expr)
          for (dependency_t * dep = form->dependencies; dep; dep = dep->next)
            if (!strcmp (dep->variable_name, expr->variable_name))
            {
              //if (expr->engine->automatic_notification)
              //expr->notify_change = 0 ;  // notify only leafs changes

              if ( /* IS_VALUED (expr->value) || */ IS_VALUED (form->value))    // TODO: check
                ASSERT ((formula_parse (form)), 0);     // The recursive stuff is here
              break;
            }
}

static int
formula_set (formula_t * const form, value_t value)
{
  int assign = 0;

  switch (value.type)
  {
    case NUMBER:
      if (IS_NUMBER (form->min_range) && GET_NUMBER (value) < GET_NUMBER (form->min_range))
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (< %g).",
                            form->formula, GET_NUMBER (form->min_range));
      else if (IS_NUMBER (form->max_range) && GET_NUMBER (value) > GET_NUMBER (form->max_range))
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (> %g).",
                            form->formula, GET_NUMBER (form->max_range));
      else
        assign = 1;
      break;
    case INTEGER:
      if (IS_INTEGER (form->min_range) && GET_INTEGER (value) < GET_INTEGER (form->min_range))
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (< %li).",
                            form->formula, GET_INTEGER (form->min_range));
      else if (IS_INTEGER (form->max_range) && GET_INTEGER (value) > GET_INTEGER (form->max_range))
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (> %li).",
                            form->formula, GET_INTEGER (form->max_range));
      else
        assign = 1;
      break;
    case DATE_TIME:
    {
      char datestr[200];

      if (IS_DATE_TIME (form->min_range) && tm_diffseconds (GET_DATE_TIME (value), GET_DATE_TIME (form->min_range)) > 0)
      {
        strftime (datestr, sizeof (datestr) / sizeof (*datestr), "%X %x", &GET_DATE_TIME (form->min_range));
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (< '%s').",
                            form->formula, datestr);
      }
      else if (IS_DATE_TIME (form->max_range)
               && tm_diffseconds (GET_DATE_TIME (value), GET_DATE_TIME (form->max_range)) < 0)
      {
        strftime (datestr, sizeof (datestr) / sizeof (*datestr), "%X %x", &GET_DATE_TIME (form->max_range));
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (> '%s').",
                            form->formula, datestr);
      }
      else
        assign = 1;
      break;
    }
    case STRING:
      if (IS_STRING (form->min_range) && strcmp (GET_STRING (value), GET_STRING (form->min_range)) < 0)
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (< \"%s\").",
                            form->formula, GET_STRING (form->min_range));
      else if (IS_STRING (form->max_range) && strcmp (GET_STRING (value), GET_STRING (form->max_range)) > 0)
        formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Out of range (> \"%s\").",
                            form->formula, GET_STRING (form->max_range));
      else
        assign = 1;
      break;
    case UNDEFINED:
    default:
      break;
  }

  if (assign)
  {
    if (value_cmp (form->value, value))
      /**/ ;
    {
      value = value_dup (value);        // Value is duplicated
      formula_unset (form);
      form->value = value;
      formula_changed (form);
    }
  }
  else
    formula_unset (form);

  return !IS_VALUED (value) || IS_VALUED (form->value);
}

static value_t parse_comparison (formula_t * const expr);
static token_t token_get (formula_t * const expr);

/**************************************************************
*                       FORMULAE PARSER                       *
**************************************************************/

/* Parser entry point */
static int
formula_parse (formula_t * const expr)
{
  if (!expr)
    return 0;

  if (!expr->formula)           // if formula is reduced to 0 (that is the case after identifier_set has been called)
    /* nothing */ ;
  else                          // if (expr->formula)
  {
    formula_free (expr, 1);     // prepare formula for reparsing : free data before update
    expr->pos = expr->formula;
    expr->value = ZERO;         // Initialize value before calling parse_comparison.
    expr->value = parse_comparison (expr);

    token_t token = token_get (expr);

    if (token.type != END)
    {
      switch (token.type)
      {
        case IDENTIFIER:
          formula_on_message (expr->engine->name, expr->formula, MSG_ERROR, 0,
                              "'%s': Unexpected identifier uncountered.", expr->formula);
          break;
        case VALUE:
          value_free (&token.attribute.value);
          formula_on_message (expr->engine->name, expr->formula, MSG_ERROR, 0, "'%s': Unexpected value uncountered.",
                              expr->formula);
          break;
        case PLUS:
        case MINUS:
        case MULTIPLY:
        case DIVIDE:
        case EQ:
        case LHPAREN:
        case RHPAREN:
        case LHBRACKET:
        case RHBRACKET:
        case ARG_SEP:
        case LT:
        case GT:
          formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                              "'%s': Unexpected '%c' uncountered.", expr->formula, token.type);
          break;
        default:
          formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                              "'%s': Unexpected end of formula uncountered.", expr->formula);
          break;
      }
      formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': At '%s'.", expr->formula,
                          expr->previous_pos);
      //formula_free(expr, 1) ;
      formula_unset (expr);
      return 0;                 // Formula end expected. The formula is not added to the engine.
    }                           // token.type != END

    if (expr->variable_name && formula_depends_on (expr, expr->variable_name))
    {
      expr->circular_dependencies = 1;
      formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0, "'%s': Circular reference.",
                          expr->formula);
      formula_unset (expr);
    }
  }                             // if (expr->formula)

  /* Adding the formula to the engine */
  if (expr->engine && expr->variable_name)
  {
    // Add the formula in the engine
    formula_t *form = 0;

    for (form = expr->engine->formulae; form && form != expr; form = form->next)
      /* nothing */ ;

    if (!form)                  // the formula is not in the engine yet
    {
      formula_t *previous = 0;

      for (form = expr->engine->formulae; form && strcmp (form->variable_name, expr->variable_name); form = form->next)
        previous = form;

      if (form)
      {
        // The engine already contains another formula with the same variable name -> it is replaced by the new one.
        // Replace 'form' by 'expr' in the list 'expr->engine->formulae' (type formula_t).
        if (form->dependencies)
          formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0, "'%s': Overwritten !",
                              form->formula);
        expr->next = form->next;
        if (previous)
          previous->next = expr;
        else
          expr->engine->formulae = expr;

        formula_free (form, 0); // 'form is not needed anymore
      }
      else
      {
        // Adding the formula at the beginning of the list 'expr->engine->formulae' (type formula_t).
        expr->next = expr->engine->formulae;
        expr->engine->formulae = expr;
      }
    }                           // The formula is not in the engine yet
  }                             // Adding the formula to the engine

#if 1
  formula_set (expr, expr->value);      // Controls that the value is in the valid range for formula expr
#endif

#if 0
  // TODO: check what is this code for
  if (IS_VALUED (expr->value))  // Avoids recursive infinite loop with formula_parse
    formula_set (expr, expr->value);    // Controls that the value is in the valid range for formula expr
  else                          // UNDEF
    formula_changed (expr);     // The recursive stuff is here
#endif

  return 1;
}

/********************************************************************
 LEXER
 Grammar:
%tokens%

  IDENTIFIER
  VALUE
  PLUS          = '+'
  MINUS         = '-'
  MULTIPLY      = '*'
  DIVIDE        = '/'
  ASSIGN        = '='
  LHPAREN       = '('
  RHPAREN       = ')'
  LHBRACKET     = '['
  RHBRACKET     = ']'
  COMMA         = ','
  WHITESPACE

%productions%

Formula = IDENTIFIER '=' Comparison ;

Comparison = Expression
           | Expression '=' Expression
           | Expression '<>' Expression
           | Expression '<' Expression
           | Expression '>' Expression ;
           | Expression '<=' Expression
           | Expression '>=' Expression ;

Expression = Term
           | Expression '+' Term
           | Expression '-' Term ;

Term = Primary
     | Term '/' Primary
     | Term '*' Primary ;

Primary = Atom
        | '-' Atom
        | '+' Atom

Atom = VALUE
     | IDENTIFIER
     | '(' Expression ')' ;
     | '[' Expression ']' ;
     | UnaryFunction
     | BinaryFunction ;

VALUE = <number>
      | '"' <string> '"'
      | ''' <date time> '''

UnaryFunction = IDENTIFIER "(" Expression ")" ;

BinaryFunction = IDENTIFIER "(" Expression ";" Expression ")" ;
***************************************************************/

static int
isid (const char *nptr, char **endptr)
{
  if (isalpha (*nptr) || *nptr == '_')  // An identifier can start with character '_'
  {
    while (isalnum (*nptr) || *nptr == '_')     // An identifier can contain character '_'
      nptr++;
    if (endptr)
      *endptr = (char *) nptr;
    return 1;
  }
  else
  {
    if (endptr)
      *endptr = (char *) nptr;
    return 0;
  }
}

/* Token factory */
static token_t
token_get (formula_t * const expr)
{
  token_t token;

  token.type = NONE;
  if (!expr || !formula_isvalid (expr))
    return token;

  expr->previous_pos = expr->pos;

  /* Ignores whitespaces */
  while (*(expr->pos) && isspace (*(expr->pos)))
    expr->pos++;

  char ch;

  switch (ch = *(expr->pos))
  {
    case '\0':
    case '+':
    case '-':
    case '*':
    case '/':
    case '=':
    case ')':
    case '(':
    case ']':
    case '[':
    case ';':
      token.type = ch;
      expr->pos++;
      return token;
    case '<':
      expr->pos++;
      if (*(expr->pos) == '>')
        token.type = NEQ;
      else if (*(expr->pos) == '=')
        token.type = LE;
      else
      {
        expr->pos--;
        token.type = ch;
      }
      expr->pos++;
      return token;
    case '>':
      expr->pos++;
      if (*(expr->pos) == '=')
        token.type = GE;
      else
      {
        expr->pos--;
        token.type = ch;
      }
      expr->pos++;
      return token;
    case '"':
    {
      size_t length = 0;

      while (*((expr->pos) + length + 1) && *((expr->pos) + length + 1) != '"')
        length++;

      if (!*((expr->pos) + length + 1))
        formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': Missing \".", expr->formula);
      else
      {
        token.type = VALUE;
        char *v = malloc ((length + 1) * sizeof (*v));

        CHECK_ALLOC (v);
        if (length)
          strncpy (v, expr->pos + 1, length);
        *(v + length) = 0;
        token.attribute.value = __MAKE_STRING (v, 0);
        expr->pos = (expr->pos) + length + 2;
        return token;
      }
    }
      break;
    case '\'':
      // Try to read a date
    {
      size_t length = 0;

      while (*((expr->pos) + length + 1) && *((expr->pos) + length + 1) != '\'')
        length++;

      if (!*((expr->pos) + length + 1))
        formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': Missing '.", expr->formula);
      else
      {
        char *s = strndup (expr->pos + 1, length);

        CHECK_ALLOC (s);

        struct tm v;

        tm_maketoday (&v);

        char *saveptr, *tok;

        if ((tok = strtok_r (s, " \t", &saveptr)) == 0 || tm_setdatefromstring (&v, tok) != TM_OK)
          formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': Incorrect date %s.",
                              expr->formula, s);
        else if ((tok = strtok_r (0, " ", &saveptr)) != 0 && tm_settimefromstring (&v, tok) != TM_OK)
          formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': Incorrect time %s.",
                              expr->formula, s);
        else
        {
          expr->pos = (expr->pos) + length + 2;
          free (s);
          token.type = VALUE;
          token.attribute.value = MAKE_DATE_TIME (v);
          return token;
        }

        free (s);
      }
    }
      break;
    default:
    {
      // Try to read an integer
      char *endl = 0;
      long int valuel = strtol (expr->pos, &endl, 0);

      // Try to read a number
      char *endd = 0;
      double valued = strtod (expr->pos, &endd);

      if (endd == expr->pos)
      {                         // it's a variable name
        if (isid (expr->pos, &endd))
        {
          token.type = IDENTIFIER;
          token.attribute.id.begin = expr->pos;
          token.attribute.id.end = expr->pos = endd;
          return token;
        }
      }
      else if (endd > endl)
      {                         // it's a number
        expr->pos = endd;
        token.type = VALUE;
        token.attribute.value = MAKE_NUMBER (valued);
        return token;
      }
      else
      {                         // it's an integer
        expr->pos = endl;
        token.type = VALUE;
        token.attribute.value = MAKE_INTEGER (valuel);
        return token;
      }
      break;
    }
  }

  formula_invalidate (expr);
  //value_free (&expr->value);
  formula_unset (expr);
  formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': Syntax error.", expr->formula);
  return token;                 // NONE
}

static void
token_unget (formula_t * const expr)
{
  if (expr)
    expr->pos = expr->previous_pos;
}

/**************************************************************
*                       FORMULAE PARSER                       *
**************************************************************/

/* Parse primary */
static value_t
parse_primary (formula_t * const expr)
{
  token_t tok = token_get (expr);

  if (!expr || !formula_isvalid (expr))
  {
    if (tok.type == VALUE)
      value_free (&tok.attribute.value);
    return UNDEF;
  }

  switch (tok.type)
  {
    case PLUS:
      return parse_primary (expr);
      break;
    case MINUS:
    {
      value_t ret = parse_primary (expr);

      if (IS_VALUED (ret))
      {
        if (IS_NUMBER (ret) && IS_VALUED (expr->value))
        {
          GET_NUMBER (ret) = -GET_NUMBER (ret);
          return ret;
        }
        else if (IS_INTEGER (ret) && IS_VALUED (expr->value))
        {
          if (INT_NEGATE_OVERFLOW (GET_INTEGER (ret)))
            /* TODO */ ASSERT (0, 0);
          // same as out of range
          else
          {
            GET_INTEGER (ret) = -GET_INTEGER (ret);
            return ret;
          }
        }
        else
        {
          formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                              "'%s': Operator '-' applied to something not a number.", expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }

      return ret;
      break;
    }
    case VALUE:
      return tok.attribute.value;
      break;
    case LHPAREN:
    {
      value_t value = parse_comparison (expr);
      token_t t = token_get (expr);

      if (t.type != RHPAREN)
      {
        if (t.type == VALUE)
          value_free (&t.attribute.value);
        formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                            "'%s': Missing right hand parenthesis.", expr->formula);
        value_free (&value);
      }
      else
        return value;
      break;
    }
    case LHBRACKET:
    {
      value_t value = parse_comparison (expr);
      token_t t = token_get (expr);

      if (t.type != RHBRACKET)
      {
        if (t.type == VALUE)
          value_free (&t.attribute.value);
        formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                            "'%s': Missing right hand bracket.", expr->formula);
        value_free (&value);
      }
      else
        return value;
      break;
    }
    case IDENTIFIER:
    {
      token_t tpar = token_get (expr);

      if (tpar.type == LHPAREN) // An identifier followed by a left-hand parenthesis should be a function name.
      {
        int found = 0;
        value_t returned_value = UNDEF;
        token_t t;
        value_t *args = 0;
        unsigned int nbArgs = 0;

        if ((t = token_get (expr)).type != RHPAREN)
        {
          if (t.type == VALUE)
            value_free (&t.attribute.value);
          token_unget (expr);

          // TODO: args(i=i..j..k ; <formula(i)>), returns a list of value_t
          do
          {
            value_t arg = parse_comparison (expr);

            nbArgs++;
            args = realloc (args, nbArgs * sizeof (*args));
            CHECK_ALLOC (args);
            args[nbArgs - 1] = arg;
            t = token_get (expr);
            if (t.type == VALUE)
              value_free (&t.attribute.value);
          }
          while (t.type == ARG_SEP);
        }

        if (t.type != RHPAREN)
          formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                              "'%s': Missing right hand parenthesis.", expr->formula);

        else if (nbArgs == 1 &&
                 strlen (GET_VALUE_FUNCTION) == tok.attribute.id.end - tok.attribute.id.begin &&
                 !strncmp (GET_VALUE_FUNCTION, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin))
        {                       // Special unary function 'value'
          //TODO: accept value(<comparison> returning a string as an identifier)
          found = 1;
          if (!IS_VALUED (args[0]))
            /* nothing */ ;
          else if (!IS_STRING (args[0]))
            formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                                "'%s': String argument expected for function '%s'.", expr->formula, GET_VALUE_FUNCTION);
          else                  //if (IS_STRING(args[0]))
          {
            int go_on = 1;

            char *endptr = 0;

            if (!isid (GET_STRING (args[0]), &endptr) || *endptr != 0)
            {
              formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                  "'%s': Argument '%s' is not a variable name for function '%s'.", expr->formula,
                                  GET_STRING (args[0]), GET_VALUE_FUNCTION);
              go_on = 0;
            }

            // Is argument the name of a constant ?
            for (constant_t * c = expr->engine->constants; go_on && c; c = c->next)
              if (strlen (c->name) == strlen (GET_STRING (args[0])) && !strcmp (c->name, GET_STRING (args[0])))
              {
                formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                    "'%s': Constant name argument '%s' not allowed for function '%s'.", expr->formula,
                                    c->name, GET_VALUE_FUNCTION);
                go_on = 0;
              }

            // Add identifier to dependencies
            if (go_on)
            {
              dependency_t *dep;

              for (dep = expr->dependencies;
                   dep && (strlen (dep->variable_name) != strlen (GET_STRING (args[0]))
                           || strcmp (dep->variable_name, GET_STRING (args[0]))); dep = dep->next)
                /* nothing */ ;

              if (!dep)
              {
                dependency_t *new_dep = malloc (sizeof (*new_dep));

                CHECK_ALLOC (new_dep);
                ASSERT (new_dep->variable_name = mystrdup (GET_STRING (args[0])), 0);
                new_dep->next = expr->dependencies;
                expr->dependencies = new_dep;
              }

              // Get the value of the identifier
              if (expr->engine)
                for (formula_t * formula = expr->engine->formulae; formula; formula = formula->next)
                  if (IS_VALUED (formula->value) && !strcmp (formula->variable_name, GET_STRING (args[0])))
                    returned_value = value_dup (formula->value);
            }
          }
          //The identifier has no value yet
          if (!IS_VALUED (returned_value))
            value_free (&expr->value);
        }

        else                    // not function value
        {
          fnargs_t *fn = 0;

          for (fn = expr->engine->fnargs;
               fn &&
               (strlen (fn->name) != tok.attribute.id.end - tok.attribute.id.begin ||
                strncmp (fn->name, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin));
               fn = fn->next)
            /* nothing */ ;

          if (fn)
          {
            if (fn->nbArgs.min < 0)
              found = 1;
            else if (fn->nbArgs.min == fn->nbArgs.max && fn->nbArgs.min != nbArgs)
              formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                                  "'%s': Function %s: uncorrect number of arguments (%i expected.)", expr->formula,
                                  fn->name, fn->nbArgs.min);
            else if (fn->nbArgs.min > nbArgs)
              formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                                  "'%s': Function %s: too few arguments (at least %i expected.)", expr->formula,
                                  fn->name, fn->nbArgs.min);
            else if (fn->nbArgs.max && fn->nbArgs.max < nbArgs)
              formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0,
                                  "'%s': Function %s: too many arguments (at most %i expected.)", expr->formula,
                                  fn->name, fn->nbArgs.max);
            else
              found = 1;

            if (!found)
              /* nothing */ ;
            else if (!IS_VALUED (expr->value))
              /* nothing */ ;
            else if (IS_NUMBER (returned_value = fn->value (nbArgs, args)) && isnan (GET_NUMBER (returned_value)))
            {
              formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                  "'%s': Function %s: argument out of range.", expr->formula, fn->name);
              formula_unset (expr);
            }
          }
          else
          {
            switch (nbArgs)
            {
              case 0:
              {
                f0arg_t *f0 = 0;

                for (f0 = expr->engine->f0args;
                     f0 && (strlen (f0->name) != tok.attribute.id.end - tok.attribute.id.begin ||
                            strncmp (f0->name, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin));
                     f0 = f0->next)
                  /* nothing */ ;

                if (f0)
                {
                  found = 1;
                  if (!IS_VALUED (expr->value))
                    /* nothing */ ;
                  else if (!isnan (f0->value ()))
                    returned_value = MAKE_NUMBER (f0->value ());
                  else
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: not a number.", expr->formula, f0->name);
                    formula_unset (expr);
                  }
                }
                break;
              }
              case 1:
              {
                f1arg_t *f1 = 0;

                for (f1 = expr->engine->f1args;
                     f1 && (strlen (f1->name) != tok.attribute.id.end - tok.attribute.id.begin ||
                            strncmp (f1->name, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin));
                     f1 = f1->next)
                  /* nothing */ ;

                if (f1)
                {
                  found = 1;
                  if (!IS_NUMBER (args[0]))
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: argument is not a number.", expr->formula, f1->name);
                    formula_unset (expr);
                  }
                  else if (!IS_VALUED (expr->value))
                    /* nothing */ ;
                  else if (!isnan (f1->value (GET_NUMBER (args[0]))))
                    returned_value = MAKE_NUMBER (f1->value (GET_NUMBER (args[0])));
                  else
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: argument (%g) out of range.", expr->formula, f1->name,
                                        GET_NUMBER (args[0]));
                    formula_unset (expr);
                  }
                }
                break;
              }
              case 2:
              {
                f2args_t *f2 = 0;

                for (f2 = expr->engine->f2args;
                     f2 && (strlen (f2->name) != tok.attribute.id.end - tok.attribute.id.begin ||
                            strncmp (f2->name, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin));
                     f2 = f2->next)
                  /* nothing */ ;

                if (f2)
                {
                  found = 1;
                  if (!IS_NUMBER (args[0]) || !IS_NUMBER (args[1]))
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: argument is not a number.", expr->formula, f2->name);
                    formula_unset (expr);
                  }
                  else if (!IS_VALUED (expr->value))
                    /* nothing */ ;
                  else if (!isnan (f2->value (GET_NUMBER (args[0]), GET_NUMBER (args[1]))))
                    returned_value = MAKE_NUMBER (f2->value (GET_NUMBER (args[0]), GET_NUMBER (args[1])));
                  else
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: arguments (%g, %g) out of range.", expr->formula, f2->name,
                                        GET_NUMBER (args[0]), GET_NUMBER (args[1]));
                    formula_unset (expr);
                  }
                }
                break;
              }
              case 3:
              {
                f3args_t *f3 = 0;

                for (f3 = expr->engine->f3args;
                     f3 && (strlen (f3->name) != tok.attribute.id.end - tok.attribute.id.begin ||
                            strncmp (f3->name, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin));
                     f3 = f3->next)
                  /* nothing */ ;

                if (f3)
                {
                  found = 1;
                  if (!IS_NUMBER (args[0]) || !IS_NUMBER (args[1]) || !IS_NUMBER (args[2]))
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: argument is not a number.", expr->formula, f3->name);
                    formula_unset (expr);
                  }
                  else if (!IS_VALUED (expr->value))
                    /* nothing */ ;
                  else if (!isnan (f3->value (GET_NUMBER (args[0]), GET_NUMBER (args[1]), GET_NUMBER (args[2]))))
                    returned_value =
                      MAKE_NUMBER (f3->value (GET_NUMBER (args[0]), GET_NUMBER (args[1]), GET_NUMBER (args[2])));
                  else
                  {
                    formula_on_message (expr->engine->name, expr->variable_name, MSG_WARNING, 0,
                                        "'%s': Function %s: argument (%g, %g, %g) out of range.", expr->formula,
                                        f3->name, GET_NUMBER (args[0]), GET_NUMBER (args[1]), GET_NUMBER (args[2]));
                    formula_unset (expr);
                  }
                }
                break;
              }
            }                   // switch (nbArgs)

            if (!found)
            {
              char *fname = mystrndup (tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin);

              formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR,
                                  0, "'%s': Unknown function name %s with %i arguments.", expr->formula, fname, nbArgs);
              free (fname);
              formula_unset (expr);
            }
          }
        }

        for (int iArg = 0; iArg < nbArgs; iArg++)
          value_free (&args[iArg]);
        free (args);
        if (found)
          return returned_value;
        else
          return returned_value;        // TODO: to be removed ?
      }                         // if (token_get(expr).type == LHPAREN)
      else                      // if (tpar.type != LHPAREN)
      {
        if (tpar.type == VALUE)
          value_free (&tpar.attribute.value);

        token_unget (expr);     // rewind, this is not a function name

        // Is it a constant ?
        for (constant_t * c = expr->engine->constants; c; c = c->next)
          if (strlen (c->name) == tok.attribute.id.end - tok.attribute.id.begin &&
              !strncmp (c->name, tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin))
            return value_dup (c->value);

        // Add identifier to dependencies
        dependency_t *dep;

        for (dep = expr->dependencies;
             dep && (strlen (dep->variable_name) != tok.attribute.id.end - tok.attribute.id.begin ||
                     strncmp (dep->variable_name, tok.attribute.id.begin,
                              tok.attribute.id.end - tok.attribute.id.begin)); dep = dep->next)
          /* nothing */ ;

        if (!dep)
        {
          dependency_t *new_dep = malloc (sizeof (*new_dep));

          CHECK_ALLOC (new_dep);
          ASSERT (new_dep->variable_name =
                  mystrndup (tok.attribute.id.begin, tok.attribute.id.end - tok.attribute.id.begin), 0);
          new_dep->next = expr->dependencies;
          expr->dependencies = new_dep;
        }

        // Get the value of the identifier
        if (expr->engine)
          for (formula_t * formula = expr->engine->formulae; formula; formula = formula->next)
            if (IS_VALUED (formula->value)
                && strlen (formula->variable_name) == tok.attribute.id.end - tok.attribute.id.begin
                && !strncmp (formula->variable_name, tok.attribute.id.begin,
                             tok.attribute.id.end - tok.attribute.id.begin))
              return value_dup (formula->value);

        formula_unset (expr);
        return UNDEF;           // The identifier has no value yet
      }
      break;
    }                           // case IDENTIFIER
    default:
      //formula_on_message(expr->engine->name, expr->formula, MSG_ERROR, "Uncomplete formula.") ;
      break;
  }

  formula_unset (expr);
  formula_invalidate (expr);
  formula_on_message (expr->engine->name, expr->variable_name, MSG_ERROR, 0, "'%s': Syntax error.", expr->formula);
  return UNDEF;
}

/* Parse term */
static value_t
parse_term (formula_t * const expr)
{
  value_t left = parse_primary (expr);

  if (!expr || !formula_isvalid (expr))
  {
    value_free (&left);
    return UNDEF;
  }

  for (;;)                      // a term is actually a succession of primaries
    // Term = Primary
    //      | Term '/' Primary
    //      | Term '*' Primary ;

  {
    token_t token = token_get (expr);

    switch (token.type)
    {
      case MULTIPLY:
      {
        value_t right = parse_primary (expr);

        if (IS_VALUED (expr->value))    // TODO: if (IS_VALUED (right))
        {
          if ((!IS_NUMBER (left) && !IS_INTEGER (left)) || (!IS_NUMBER (right) && !IS_INTEGER (right)))
          {
            formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                                expr->formula);
            formula_unset (expr);
            value_free (&left);
          }
          else if (IS_INTEGER (left) && IS_INTEGER (right))
          {
            if (INT_MULTIPLY_OVERFLOW (GET_INTEGER (left), GET_INTEGER (right)))
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Out of range (%li*%li).",
                                  expr->formula, GET_INTEGER (left), GET_INTEGER (right));
              formula_unset (expr);
              value_free (&left);
            }
            else
              GET_INTEGER (left) *= GET_INTEGER (right);
          }
          else
          {
            double l = IS_INTEGER (left) ? GET_INTEGER (left) : GET_NUMBER (left);
            double r = IS_INTEGER (right) ? GET_INTEGER (right) : GET_NUMBER (right);

            if (l == 0. || r == 0.)
            {
              value_free (&left);
              left.type = NUMBER;
              GET_NUMBER (left) = 0.;
            }
            else if (isnormal (l * r))
            {
              value_free (&left);
              left.type = NUMBER;
              GET_NUMBER (left) = l * r;
            }
            else
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Out of range (%g*%g).",
                                  expr->formula, GET_NUMBER (left), GET_NUMBER (right));
              formula_unset (expr);
              value_free (&left);
            }
          }
        }
        else if (IS_VALUED (left))
          value_free (&left);

        value_free (&right);
        // No return here (we are in a loop). Return is in the default case.
        break;
      }
      case DIVIDE:
      {
        value_t right = parse_primary (expr);

        if (IS_VALUED (expr->value))
        {
          if ((!IS_NUMBER (left) && !IS_INTEGER (left)) || (!IS_NUMBER (right) && !IS_INTEGER (right)))
          {
            formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                                expr->formula);
            formula_unset (expr);
            value_free (&left);
          }
          else if (IS_INTEGER (left) && IS_INTEGER (right))
          {
            if (GET_INTEGER (right) == 0L)
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Division by zero.",
                                  expr->formula);
              formula_unset (expr);
              value_free (&left);
            }
            else if (!INT_REMAINDER_OVERFLOW (GET_INTEGER (left), GET_INTEGER (right))
                     && GET_INTEGER (left) % GET_INTEGER (right))
            {
              value_free (&left);
              left.type = NUMBER;
              GET_NUMBER (left) = (1. * GET_INTEGER (left)) / (1. * GET_INTEGER (right));
            }
            else if (INT_DIVIDE_OVERFLOW (GET_INTEGER (left), GET_INTEGER (right)))     // In case right == -1
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Out of range (%li/%li).",
                                  expr->formula, GET_INTEGER (left), GET_INTEGER (right));
              formula_unset (expr);
              value_free (&left);
            }
            else
              GET_INTEGER (left) /= GET_INTEGER (right);
          }
          else
          {
            double l = IS_INTEGER (left) ? GET_INTEGER (left) : GET_NUMBER (left);
            double r = IS_INTEGER (right) ? GET_INTEGER (right) : GET_NUMBER (right);

            if (r == 0.)
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Division by zero.",
                                  expr->formula);
              formula_unset (expr);
              value_free (&left);
            }
            else if (l == 0.)
            {
              value_free (&left);
              left.type = NUMBER;
              GET_NUMBER (left) = 0.;
            }
            else if (isnormal (l / r))
            {
              value_free (&left);
              left.type = NUMBER;
              GET_NUMBER (left) = l / r;
            }
            else
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Out of range (%g/%g).",
                                  expr->formula, GET_NUMBER (left), GET_NUMBER (right));
              formula_unset (expr);
              value_free (&left);
            }

          }
        }
        else if (IS_VALUED (left))
          value_free (&left);

        value_free (&right);
        break;
      }
      case NONE:
        value_free (&left);
        return UNDEF;
        break;
      case VALUE:
        value_free (&token.attribute.value);
        /* no break, drops through */
      default:
        token_unget (expr);
        return left;
        break;
    }
  }
}

/* Parse expression */
static value_t
parse_expression (formula_t * const expr)
{
  value_t left = parse_term (expr);

  if (!expr || !formula_isvalid (expr))
  {
    value_free (&left);
    return UNDEF;
  }

  for (;;)                      // an expression is actually a succession of terms
    // Expression = Term
    //            | Expression '+' Term
    //            | Expression '-' Term ;

  {
    token_t token = token_get (expr);

    switch (token.type)
    {
      case PLUS:
      {
        value_t right = parse_term (expr);

        if (IS_VALUED (expr->value))
        {
          if (IS_STRING (left))
          {
            if (IS_STRING (right))
            {
              char *v = malloc ((strlen (GET_STRING (left)) + strlen (GET_STRING (right)) + 1) * sizeof (*v));

              CHECK_ALLOC (v);
              strcpy (v, GET_STRING (left));
              strcat (v, GET_STRING (right));
              value_free (&left);
              left = __MAKE_STRING (v, 0);
            }
            else if (IS_INTEGER (right) && GET_INTEGER (right) >= 0)
            {
              const char *fmt = "%s%li";
              int len = snprintf (0, 0, fmt, GET_STRING (left), GET_INTEGER (right));
              char *v = 0;

              if (len >= 0)
              {
                len++;
                v = malloc (len * sizeof (*v));
                CHECK_ALLOC (v);
                snprintf (v, len, fmt, GET_STRING (left), GET_INTEGER (right));
              }
              value_free (&left);
              left = __MAKE_STRING (v, 0);
            }
            else
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0,
                                  "'%s': Operation not allowed (%s is not a positive integer or a string).",
                                  expr->formula, value_to_string (right));
              formula_unset (expr);
              value_free (&left);
            }
          }
          else if (IS_NUMBER (left) && IS_NUMBER (right))
            GET_NUMBER (left) += GET_NUMBER (right);
          else if (IS_NUMBER (left) && IS_INTEGER (right))
            GET_NUMBER (left) += GET_INTEGER (right);
          else if (IS_INTEGER (left) && IS_NUMBER (right))
          {
            value_free (&left);
            GET_NUMBER (left) = GET_INTEGER (left) + GET_NUMBER (right);
            left.type = NUMBER;
          }
          else if (IS_INTEGER (left) && IS_INTEGER (right))
          {
            if (INT_ADD_OVERFLOW (GET_INTEGER (left), GET_INTEGER (right)))
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Out of range (%li+%li).",
                                  expr->formula, GET_INTEGER (left), GET_INTEGER (right));
              formula_unset (expr);
              value_free (&left);
            }
            else
              GET_INTEGER (left) += GET_INTEGER (right);
          }
          else if (IS_DATE_TIME (left) && IS_INTEGER (right))
            //TODO: check overflow
            tm_addseconds (&GET_DATE_TIME (left), 24 * 60 * 60 * GET_INTEGER (right));
          else
          {
            formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                                expr->formula);
            formula_unset (expr);
            value_free (&left);
          }
        }
        else if (IS_VALUED (left))
          value_free (&left);

        value_free (&right);
        break;
      }
      case MINUS:
      {
        value_t right = parse_term (expr);

        if (IS_VALUED (expr->value))
        {
          if (IS_DATE_TIME (left) && IS_INTEGER (right))
            tm_addseconds (&GET_DATE_TIME (left), -24 * 60 * 60 * GET_INTEGER (right));
          else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
            left = MAKE_INTEGER (tm_diffdays (GET_DATE_TIME (right), GET_DATE_TIME (left), 0));
          else if (IS_NUMBER (left) && IS_NUMBER (right))
            GET_NUMBER (left) -= GET_NUMBER (right);
          else if (IS_NUMBER (left) && IS_INTEGER (right))
            GET_NUMBER (left) -= GET_INTEGER (right);
          else if (IS_INTEGER (left) && IS_NUMBER (right))
          {
            value_free (&left);
            GET_NUMBER (left) = GET_INTEGER (left) - GET_NUMBER (right);
            left.type = NUMBER;
          }
          else if (IS_INTEGER (left) && IS_INTEGER (right))
          {
            if (INT_SUBTRACT_OVERFLOW (GET_INTEGER (left), GET_INTEGER (right)))
            {
              formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Out of range (%li-%li).",
                                  expr->formula, GET_INTEGER (left), GET_INTEGER (right));
              formula_unset (expr);
              value_free (&left);
            }
            else
              GET_INTEGER (left) -= GET_INTEGER (right);
          }
          else
          {
            formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                                expr->formula);
            formula_unset (expr);
            value_free (&left);
          }
        }
        else if (IS_VALUED (left))
          value_free (&left);

        value_free (&right);
        break;
      }
      case NONE:
        value_free (&left);
        return UNDEF;
        break;
      case VALUE:
        value_free (&token.attribute.value);
        /* no break, fall through */
      default:
        token_unget (expr);
        return left;
        break;
    }
  }
}

/* Parse comparison */
static value_t
parse_comparison (formula_t * const expr)
{
  value_t left = parse_expression (expr);

  if (!expr || !formula_isvalid (expr))
  {
    formula_unset (expr);
    value_free (&left);
    return UNDEF;
  }

  value_t ret = ZERO;

  token_t token = token_get (expr);

  switch (token.type)
  {
    case LT:
    {
      value_t right = parse_expression (expr);

      if (IS_VALUED (expr->value))
      {
        if (IS_NUMBER (left) && IS_NUMBER (right))
          ret = (GET_NUMBER (left) < GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_NUMBER (left) && IS_INTEGER (right))
          ret = (GET_NUMBER (left) < GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_NUMBER (right))
          ret = (GET_INTEGER (left) < GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_INTEGER (right))
          ret = (GET_INTEGER (left) < GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_STRING (left) && IS_STRING (right))
          ret = (strcmp (GET_STRING (left), GET_STRING (right)) < 0 ? ONE : ZERO);
        else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
          ret = tm_diffseconds (GET_DATE_TIME (left), GET_DATE_TIME (right)) > 0 ? ONE : ZERO;
        else
        {
          formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                              expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }
      else
        value_free (&ret);

      value_free (&right);
      value_free (&left);
      break;
    }
    case GT:
    {
      value_t right = parse_expression (expr);

      if (IS_VALUED (expr->value))
      {
        if (IS_NUMBER (left) && IS_NUMBER (right))
          ret = (GET_NUMBER (left) > GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_NUMBER (right))
          ret = (GET_INTEGER (left) > GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_NUMBER (left) && IS_INTEGER (right))
          ret = (GET_NUMBER (left) > GET_INTEGER (right) ? ONE : ZERO);
        if (IS_INTEGER (left) && IS_INTEGER (right))
          ret = (GET_INTEGER (left) > GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_STRING (left) && IS_STRING (right))
          ret = (strcmp (GET_STRING (left), GET_STRING (right)) > 0 ? ONE : ZERO);
        else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
          ret = tm_diffseconds (GET_DATE_TIME (left), GET_DATE_TIME (right)) < 0 ? ONE : ZERO;
        else
        {
          formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                              expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }
      else
        value_free (&ret);

      value_free (&right);
      value_free (&left);
      break;
    }
    case EQ:
    {
      value_t right = parse_expression (expr);

      if (IS_VALUED (expr->value))
      {
        if (IS_NUMBER (left) && IS_NUMBER (right))
          ret = (GET_NUMBER (left) == GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_NUMBER (right))
          ret = (GET_INTEGER (left) == GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_NUMBER (left) && IS_INTEGER (right))
          ret = (GET_NUMBER (left) == GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_INTEGER (right))
          ret = (GET_INTEGER (left) == GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_STRING (left) && IS_STRING (right))
          ret = (strcmp (GET_STRING (left), GET_STRING (right)) == 0 ? ONE : ZERO);
        else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
          ret = tm_diffseconds (GET_DATE_TIME (left), GET_DATE_TIME (right)) ? ZERO : ONE;
        else
        {
          formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                              expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }
      else
        value_free (&ret);

      value_free (&right);
      value_free (&left);
      break;
    }

    case LE:
    {
      value_t right = parse_expression (expr);

      if (IS_VALUED (expr->value))
      {
        if (IS_NUMBER (left) && IS_NUMBER (right))
          ret = (GET_NUMBER (left) <= GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_NUMBER (left) && IS_INTEGER (right))
          ret = (GET_NUMBER (left) <= GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_NUMBER (right))
          ret = (GET_INTEGER (left) <= GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_INTEGER (right))
          ret = (GET_INTEGER (left) <= GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_STRING (left) && IS_STRING (right))
          ret = (strcmp (GET_STRING (left), GET_STRING (right)) <= 0 ? ONE : ZERO);
        else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
          ret = tm_diffseconds (GET_DATE_TIME (left), GET_DATE_TIME (right)) >= 0 ? ONE : ZERO;
        else
        {
          formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                              expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }
      else
        value_free (&ret);

      value_free (&right);
      value_free (&left);
      break;
    }
    case GE:
    {
      value_t right = parse_expression (expr);

      if (IS_VALUED (expr->value))
      {
        if (IS_NUMBER (left) && IS_NUMBER (right))
          ret = (GET_NUMBER (left) >= GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_NUMBER (right))
          ret = (GET_INTEGER (left) >= GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_NUMBER (left) && IS_INTEGER (right))
          ret = (GET_NUMBER (left) >= GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_INTEGER (right))
          ret = (GET_INTEGER (left) >= GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_STRING (left) && IS_STRING (right))
          ret = (strcmp (GET_STRING (left), GET_STRING (right)) >= 0 ? ONE : ZERO);
        else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
          ret = tm_diffseconds (GET_DATE_TIME (left), GET_DATE_TIME (right)) <= 0 ? ONE : ZERO;
        else
        {
          formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                              expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }
      else
        value_free (&ret);

      value_free (&right);
      value_free (&left);
      break;
    }
    case NEQ:
    {
      value_t right = parse_expression (expr);

      if (IS_VALUED (expr->value))
      {
        if (IS_NUMBER (left) && IS_NUMBER (right))
          ret = (GET_NUMBER (left) != GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_NUMBER (right))
          ret = (GET_INTEGER (left) != GET_NUMBER (right) ? ONE : ZERO);
        else if (IS_NUMBER (left) && IS_INTEGER (right))
          ret = (GET_NUMBER (left) != GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_INTEGER (left) && IS_INTEGER (right))
          ret = (GET_INTEGER (left) != GET_INTEGER (right) ? ONE : ZERO);
        else if (IS_STRING (left) && IS_STRING (right))
          ret = (strcmp (GET_STRING (left), GET_STRING (right)) != 0 ? ONE : ZERO);
        else if (IS_DATE_TIME (left) && IS_DATE_TIME (right))
          ret = tm_diffseconds (GET_DATE_TIME (left), GET_DATE_TIME (right)) ? ONE : ZERO;
        else
        {
          formula_on_message (expr->engine->name, expr->formula, MSG_WARNING, 0, "'%s': Operation not allowed.",
                              expr->formula);
          formula_unset (expr);
          value_free (&ret);
        }
      }
      else
        value_free (&ret);

      value_free (&right);
      value_free (&left);
      break;
    }

    case NONE:
      value_free (&left);
      break;
    case VALUE:
      value_free (&token.attribute.value);
      /* no break, drops through */
    default:
      token_unget (expr);
      ret = left;
      break;
  }

  if (!IS_VALUED (ret))
    formula_unset (expr);

  return ret;
}

/**************************************************************
*                      PREDEFINED FUNCTIONS                   *
**************************************************************/
static double
_f_random ()
{
  static int myrandomisinit = 0;
  static struct drand48_data buffer;

  if (!myrandomisinit)
  {
    srand48_r (time (0), &buffer);
    myrandomisinit = 1;
  }

  double ret = 0;

  drand48_r (&buffer, &ret);
  return ret;
}

static double
_f_not (double x)
{
  return (x ? 0. : 1.);
}

static double
_f_sqr (double x)
{
  return x * x;
}

static value_t
_f_max (int nbArgs, const value_t * const args)
{
  if (!nbArgs)
    return UNDEF;

  int type = args[0].type;
  value_t max = args[0];

  switch (type)
  {
    case INTEGER:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (GET_INTEGER (args[iarg]) > GET_INTEGER (max))
          max = args[iarg];
      }
      return MAKE_INTEGER (GET_INTEGER (max));
    case NUMBER:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (GET_NUMBER (args[iarg]) > GET_NUMBER (max))
          max = args[iarg];
      }
      return MAKE_NUMBER (GET_NUMBER (max));
    case STRING:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (strcmp (GET_STRING (args[iarg]), GET_STRING (max)) > 0)
          max = args[iarg];
      }
      return MAKE_STRING (GET_STRING (max));
    case DATE_TIME:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (tm_diffseconds (GET_DATE_TIME (args[iarg]), GET_DATE_TIME (max)) < 0)
          max = args[iarg];
      }
      return MAKE_DATE_TIME (GET_DATE_TIME (max));
    default:
      return UNDEF;
  }
}

static value_t
_f_min (int nbArgs, const value_t * const args)
{
  if (!nbArgs)
    return UNDEF;

  int type = args[0].type;
  value_t min = args[0];

  switch (type)
  {
    case INTEGER:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (GET_INTEGER (args[iarg]) < GET_INTEGER (min))
          min = args[iarg];
      }
      return MAKE_INTEGER (GET_INTEGER (min));
    case NUMBER:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (GET_NUMBER (args[iarg]) < GET_NUMBER (min))
          min = args[iarg];
      }
      return MAKE_NUMBER (GET_NUMBER (min));
    case STRING:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (strcmp (GET_STRING (args[iarg]), GET_STRING (min)) < 0)
          min = args[iarg];
      }
      return MAKE_STRING (GET_STRING (min));
    case DATE_TIME:
      for (int iarg = 1; iarg < nbArgs; iarg++)
      {
        if (args[iarg].type != type)
          return UNDEF;
        if (tm_diffseconds (GET_DATE_TIME (args[iarg]), GET_DATE_TIME (min)) > 0)
          min = args[iarg];
      }
      return MAKE_DATE_TIME (GET_DATE_TIME (min));
    default:
      return UNDEF;
  }
}

static value_t
_f_and (int nbArgs, const value_t * const args)
{
  if (nbArgs < 2)
  {
    fprintf (stderr, "Invalid argument value nbArgs (%s, %s, %i)\n", __func__, __FILE__, __LINE__);
    exit (-1);
  }

  for (int i = 0; i < nbArgs; i++)
    if (!IS_INTEGER (args[i]))
    {
      //fprintf(stderr, "Invalid type for argument %i nbArgs (%s, %s, %i)\n",i+1, __func__,__FILE__,__LINE__);
      //exit(-1);

      return UNDEF;
    }

  int ret = (GET_INTEGER (args[0]) && GET_INTEGER (args[1]));

  for (int i = 3; i <= nbArgs; i++)
    ret = (ret && GET_INTEGER (args[i - 1]));

  return MAKE_INTEGER (ret);
}

static value_t
_f_or (int nbArgs, const value_t * const args)
{
  if (nbArgs < 2)
  {
    fprintf (stderr, "Invalid argument value nbArgs (%s, %s, %i)\n", __func__, __FILE__, __LINE__);
    exit (-1);
  }

  for (int i = 0; i < nbArgs; i++)
    if (!IS_INTEGER (args[i]))
      return UNDEF;

  int ret = (GET_INTEGER (args[0]) || GET_INTEGER (args[1]));

  for (int i = 3; i <= nbArgs; i++)
    ret = (ret || GET_INTEGER (args[i - 1]));

  return MAKE_INTEGER (ret);
}

static value_t
_f_iiff (int nbArgs, const value_t * const args)
{
  if (nbArgs != 3)
  {
    fprintf (stderr, "Invalid argument value nbArgs (%s, %s, %i)\n", __func__, __FILE__, __LINE__);
    exit (-1);
  }

  if (!IS_INTEGER (args[0]))
  {
    fprintf (stderr, "Invalid argument type (%s, %s, %i)\n", __func__, __FILE__, __LINE__);
    exit (-1);
  }
  return (GET_INTEGER (args[0]) ? args[1] : args[2]);
}

static value_t
_f_sum (int nbArgs, const value_t * const args)
{
  for (int i = 1; i < nbArgs; i++)
    if (args[0].type != args[i].type)
      return UNDEF;

  if (IS_NUMBER (args[0]))
  {
    double ret = 0.;

    for (int i = 0; i < nbArgs; i++)
      ret += GET_NUMBER (args[i]);

    return MAKE_NUMBER (ret);
  }
  else if (IS_INTEGER (args[0]))
  {
    double ret = 0.;

    for (int i = 0; i < nbArgs; i++)
      ret += GET_INTEGER (args[i]);

    return MAKE_INTEGER (ret);
  }
  else if (IS_STRING (args[0]))
  {
    size_t len = 0;

    for (int i = 0; i < nbArgs; i++)
      len += strlen (GET_STRING (args[i]));

    char *ret = malloc ((len + 1) * sizeof (*ret));

    CHECK_ALLOC (ret);
    *ret = 0;

    for (int i = 0; i < nbArgs; i++)
      strcat (ret, GET_STRING (args[i]));

    return __MAKE_STRING (ret, 0);
  }

  return ZERO;
}

static value_t
_f_to_number (int nbArgs, const value_t * const args)
{
  if (nbArgs < 1 || !IS_STRING (args[0]))
    return UNDEF;

  // Try to read an integer
  char *endl = 0;
  long int valuel = strtol (GET_STRING (args[0]), &endl, 0);

  // Try to read a number
  char *endd = 0;
  double valued = strtod (GET_STRING (args[0]), &endd);

  if (endd == GET_STRING (args[0]))
    return UNDEF;
  else if (endd > endl)
    return MAKE_NUMBER (valued);
  else
    return MAKE_INTEGER (valuel);
}

static value_t
_f_round_number (int nbArgs, const value_t * const args)
{
  if (nbArgs != 1)
    return UNDEF;

  if (IS_NUMBER (args[0]))
    return MAKE_INTEGER (lround (GET_NUMBER (args[0])));
  else if (IS_INTEGER (args[0]))
    return args[0];
  else
    return UNDEF;
}

static value_t
_f_diff_dates (int nbArgs, const value_t * const args)
{
  if (nbArgs < 2 || nbArgs > 3 || !IS_DATE_TIME (args[0]) || !IS_DATE_TIME (args[1]))
    return UNDEF;

  if (nbArgs == 3 && (!IS_STRING (args[2]) || strlen (GET_STRING (args[2])) != 1))
    return UNDEF;

  char c = nbArgs == 3 ? GET_STRING (args[2])[0] : 'D';

  struct tm begin = GET_DATE_TIME (args[0]);
  struct tm end = GET_DATE_TIME (args[1]);
  long int diff = 0;

  switch (c)
  {
    case 'Y':
      diff = tm_diffyears (begin, end, 0, 0, 0);
      break;
    case 'M':
      diff = tm_diffmonths (begin, end, 0, 0);
      break;
    case 'D':
      diff = tm_diffdays (begin, end, 0);
      break;
    case 'h':
      diff = tm_diffseconds (begin, end) / 3600;
      break;
    case 'm':
      diff = tm_diffseconds (begin, end) / 60;
      break;
    case 's':
      diff = tm_diffseconds (begin, end);
      break;
  }

  return MAKE_INTEGER (diff);
}

static value_t
_f_date_add (int nbArgs, const value_t * const args)
{
  if (nbArgs < 2 || nbArgs > 3 || !IS_DATE_TIME (args[0]) || !IS_INTEGER (args[1]))
    return UNDEF;

  if (nbArgs == 3 && (!IS_STRING (args[2]) || strlen (GET_STRING (args[2])) != 1))
    return UNDEF;

  char c = nbArgs == 3 ? GET_STRING (args[2])[0] : 'D';

  struct tm begin = GET_DATE_TIME (args[0]);
  long int diff = GET_INTEGER (args[1]);
  struct tm end = begin;

  switch (c)
  {
    case 'Y':
      tm_addyears (&end, diff);
      break;
    case 'M':
      tm_addmonths (&end, diff);
      break;
    case 'W':
      tm_adddays (&end, 7 * diff);
      break;
    case 'D':
      tm_adddays (&end, diff);
      break;
    case 'h':
      tm_addseconds (&end, diff * 3600);
      break;
    case 'm':
      tm_addseconds (&end, diff * 60);
      break;
    case 's':
      tm_addseconds (&end, diff);
      break;
  }

  return MAKE_DATE_TIME (end);
}

static value_t
_f_date_attribute (int nbArgs, const value_t * const args)
{
  if (nbArgs != 2 || !IS_DATE_TIME (args[0]) || !IS_STRING (args[1]))
    return UNDEF;

  char *arg = GET_STRING (args[1]);

  if (!strcmp (arg, "Y"))       // year
    return MAKE_INTEGER (tm_getyear (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "M"))  // month
    return MAKE_INTEGER (tm_getmonth (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "D"))  // day of month
    return MAKE_INTEGER (tm_getday (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "h"))  // hour of day
    return MAKE_INTEGER (tm_gethour (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "m"))  // minutes of hour
    return MAKE_INTEGER (tm_getminute (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "s"))  // seconds of minute
    return MAKE_INTEGER (tm_getsecond (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "sD")) // seconds of day
    return MAKE_INTEGER (tm_getsecondsofday (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "IY")) // ISO-year
    return MAKE_INTEGER (tm_getisoyear (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "IW")) // ISO-week
    return MAKE_INTEGER (tm_getisoweek (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "DW")) // day of week
    return MAKE_INTEGER (tm_getdayofweek (GET_DATE_TIME (args[0])));
  else if (!strcmp (arg, "DY")) // day of year
    return MAKE_INTEGER (tm_getdayofyear (GET_DATE_TIME (args[0])));
  else
    return UNDEF;
}

static value_t
_f_date (int nbArgs, const value_t * const args)
{
  if (nbArgs != 6)
    return UNDEF;

  for (int iarg = 0; iarg < nbArgs; iarg++)
    if (!IS_INTEGER (args[iarg]))
      return UNDEF;

  struct tm instant;

  tm_makelocal (&instant, GET_INTEGER (args[0]), GET_INTEGER (args[1]), GET_INTEGER (args[2]), GET_INTEGER (args[3]),
                GET_INTEGER (args[4]), GET_INTEGER (args[5]));
  return MAKE_DATE_TIME (instant);
}

static value_t
_f_to_string (int nbArgs, const value_t * const args)
{
  // Arguments :
  //   - 1st: value to convert to string (compulsory, number or string)
  //   - 2nd: conversion specifier (optional, string of zero or more of 'e', 'E', 'f', 'F', 'g', 'G', all ignored except last valid character, default "g")
  //   - 3rd: precision (optional, number, default 6)
  //   - 4th: field width (optional, number, default is minimal width, a too small field width is ignored and does not cause any truncation)
  //   - 5th: flags (optional, string of zero or more of '0', '-', ' ', '+', default "")
  //     - '0': padded on the left with zeros rather than blanks.
  //     - '-': left adjusted on the field boundary (the default is right justification.). Overrides a '0'.
  //     - ' ' (a  space): a blank is left before a positive number
  //     - '+': a sign + is left before a positive number. Overrides a ' '.
  //
  // See fprintf usage for a complete description.

  if (!nbArgs)
    return UNDEF;
  else if (IS_STRING (args[0]))
    return value_dup (args[0]);
  else if (IS_DATE_TIME (args[0]))
  {
    char s[200];

    if (nbArgs > 1 && IS_STRING (args[1]))
      strftime (s, sizeof (s) / sizeof (*s), GET_STRING (args[1]), &GET_DATE_TIME (args[0]));
    else
      strftime (s, sizeof (s) / sizeof (*s), "%x %X", &GET_DATE_TIME (args[0]));

    return MAKE_STRING (s);
  }
  else if (!IS_INTEGER (args[0]) && !IS_NUMBER (args[0]))
    return UNDEF;

  // 1. Get format from arguments:
  const char formatd[] = "eEfFgG";
  const char formatl[] = "diouxX";
  const char flags[] = "0- +";
  const char DEFAULT_FORMATD[] = "*.*g";
  const char DEFAULT_FORMATL[] = "*.*li";

  char *format = calloc (1 + strlen (flags) + strlen (IS_NUMBER (args[0]) ? DEFAULT_FORMATD : DEFAULT_FORMATL) + 1,
                         sizeof (*format));

  CHECK_ALLOC (format);
  *format = '%';                // format head

  // 1.1. precision:
  int precision = 6;            // default precision

  if (nbArgs >= 3 && IS_INTEGER (args[2]))
    precision = GET_INTEGER (args[2]);

  // 1.2. field width:
  int width = 0;                // default field width

  if (nbArgs >= 4 && IS_INTEGER (args[3]))
    width = GET_INTEGER (args[3]);

  // 1.3. flags:
  if (nbArgs >= 5 && IS_STRING (args[4]))
    for (int i = strlen (flags); i; i--)
      if (index (GET_STRING (args[4]), flags[i - 1]))
        format[strlen (format)] = flags[i - 1]; // Appends

  // 1.4. conversion specifier:
  strcat (format, IS_NUMBER (args[0]) ? DEFAULT_FORMATD : DEFAULT_FORMATL);     // format tail, default conversion specifier is 'g' or 'i'
  if (nbArgs >= 2 && IS_STRING (args[1]))
    for (int i = strlen (GET_STRING (args[1])); i; i--)
    {
      char *pc;

      if ((pc = index (IS_NUMBER (args[0]) ? formatd : formatl, GET_STRING (args[1])[i - 1])))
      {
        format[strlen (format) - 1] = *pc;      // Overwrites
        break;
      }
    }

  // 2. Convert to string using format:
  char *v = 0;
  int len = snprintf (0, 0, format, width, precision, GET_NUMBER (args[0]));

  if (len >= 0)
  {
    len++;
    CHECK_ALLOC (v = malloc (len * sizeof (*v)));
    snprintf (v, len, format, width, precision, GET_NUMBER (args[0]));
  }

  // 3. Dispose and return:
  free (format);
  return __MAKE_STRING (v, 0);
}

static value_t
_f_now (int nbArgs, const value_t * const args)
{
  if (nbArgs)
    return UNDEF;

  struct tm d;

  tm_makenow (&d);

  return MAKE_DATE_TIME (d);
}

static value_t
_f_today (int nbArgs, const value_t * const args)
{
  if (nbArgs)
    return UNDEF;

  struct tm d;

  tm_maketoday (&d);

  return MAKE_DATE_TIME (d);
}

// TODO: LOOP i=i..j..k <id(i)> = formula(i)>
// TODO: identifier=search(i=1..3..19;<condition(i)>;<expression(i)>)

/**************************************************************
*                      ENGINES MANAGER                        *
**************************************************************/
inline static engine_t *
engine_get (const char *engine_name)
{
  engine_t *engine;

  for (engine = theEngines; engine && strcmp (engine->name, engine_name); engine = engine->next)
    /* nothing */ ;

  return engine;
}

inline static formula_t *
engine_identifier_get (const char *engine_name, const char *identifier)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  // Add the formula in the engine
  formula_t *form = 0;

  for (form = engine->formulae; form && strcmp (form->variable_name, identifier); form = form->next)
    /* nothing */ ;

/*  if (!form)*/

/*    formula_on_message(engine_name, identifier, MSG_ERROR, "Unknown variable.") ;*/

  return form;
}

inline static size_t
engine_size (const char *engine_name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  size_t size = 0;

  for (formula_t * f = engine->formulae; f; f = f->next)
    size++;

  return size;
}

/**************************************************************
*    OPTIMIZATION FUNCTIONS (MINIMIZER, SOLVER) USING GSL     *
**************************************************************/

/*
static void gsl_handler (const char * reason, const char * file, int line, int gsl_errno)
{
}
*/

/*****************************************************/

typedef struct list_of_dimension
{
  formula_t *variable;
  struct list_of_dimension *next;
} dimension_t;

typedef struct
{
  dimension_t *degrees_of_freedom;
  formula_t *minimize;
} minimize_parameter_t;

typedef struct
{
  dimension_t *degrees_of_freedom;
  dimension_t *nullify;
} rootfind_parameter_t;

static double
x_to_f (formula_t * form, double x)
{
  return x;
}

static double
f_to_x (formula_t * form)
{
  return GET_NUMBER (form->value);
}

/*****************************************************/

static double
gsl_minimizer (const gsl_vector * v, void *p)
{
  minimize_parameter_t *params = (minimize_parameter_t *) p;
  int i = 0;

  for (dimension_t * d = params->degrees_of_freedom; d; d = d->next)
  {
    formula_t *form = d->variable;

    if (!formula_set (form, MAKE_NUMBER (x_to_f (form, gsl_vector_get (v, i)))))
      return GSL_NAN;
    i++;
  }
  value_t val;

  if (!formula_get (params->minimize, &val))
    return GSL_NAN;
  return GET_NUMBER (val);
}

static int
v_engine_minimize (const char *engine_name, const char *variable_to_minimize, size_t freedom_size, const char *vargs[])
{
  //gsl_set_error_handler (gsl_handler);
  gsl_set_error_handler_off ();

  int RESTART_ITER = 3;
  const int MAX_ITER = 1000;

  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  minimize_parameter_t params;
  formula_t *form = engine_identifier_get (engine_name, variable_to_minimize);

  if (!form)
  {
    formula_on_message (engine_name, variable_to_minimize, MSG_ERROR, 0, "Undefined variable name.");
    return 0;
  }
  else if (!formula_get (form, 0))
    return 0;
  else
  {
    params.minimize = form;
    formula_on_message (engine_name, variable_to_minimize, MSG_INFO, 0, "Minimization.");
  }

  size_t fs = 0;

  params.degrees_of_freedom = 0;
  dimension_t *last_df = 0;

  for (size_t i = 0; i < freedom_size; i++)
  {
    const char *arg = vargs[i];
    formula_t *f;

    if (!(f = engine_identifier_get (engine_name, arg)))
      formula_on_message (engine_name, arg, MSG_ERROR, 0, "Undefined variable. Ignored.");
    else if (!formula_depends_on (form, arg))
      formula_on_message (engine_name, form->variable_name, MSG_WARNING, 0, "'%s': Independant of %s. Ignored.",
                          form->formula, arg);
    else if (f->dependencies)
      formula_on_message (engine_name, form->variable_name, MSG_WARNING, 0,
                          "'%s': Depends on other variables. Ignored.", form->formula);
    else
    {
      fs++;
      dimension_t *d = malloc (sizeof (*d));

      CHECK_ALLOC (d);
      formula_on_message (engine_name, arg, MSG_INFO, 0, "Degree(s) of freedom.");
      d->variable = f;
      d->next = 0;
      if (!params.degrees_of_freedom)
        params.degrees_of_freedom = last_df = d;
      else
      {
        last_df->next = d;
        last_df = d;
      }
    }
  }

  if (!fs)
    return 0;

  const gsl_multimin_fminimizer_type *T = gsl_multimin_fminimizer_nmsimplex2;

  gsl_multimin_function minex_func;

  /* Initialize method and iterate */
  minex_func.n = fs;
  minex_func.f = gsl_minimizer;
  minex_func.params = (void *) &params;

  /* Starting point */
  gsl_vector *x = gsl_vector_alloc (fs);

  /* Set initial step sizes to 1 */
  gsl_vector *ss = gsl_vector_alloc (fs);
  gsl_multimin_fminimizer *s = gsl_multimin_fminimizer_alloc (T, fs);

  CHECK_ALLOC (x);
  CHECK_ALLOC (ss);
  CHECK_ALLOC (s);

  dimension_t *d = params.degrees_of_freedom;

  for (size_t i = 0; i < fs; i++)
  {
    value_t value;

    if (formula_get (d->variable, &value) && IS_NUMBER (value)
        && identifier_set (engine_name, d->variable->variable_name, value))
      gsl_vector_set (x, i, f_to_x (d->variable));
    else
    {
      formula_on_message (engine_name, d->variable->variable_name, MSG_WARNING, 0, "Invalid starting point.");
      RESTART_ITER = 0;
    }

    /*
       if (d->variable->valued)
       gsl_vector_set (x, i, f_to_x(d->variable)) ;
       else
       {
       formula_on_message(engine_name, d->variable->variable_name, MSG_INFO, "%s has no value yet.", d->variable->variable_name) ;
       RESTART_ITER = 0 ;
       }
     */

    gsl_vector_set (ss, i, 0.1);        // !! Valeur en dur ici. A voir...
    d = d->next;
  }

  int quiet = engine->quiet;

  engine->quiet = 1;

  if (gsl_minimizer (x, &params) == GSL_NAN)
  {
    formula_on_message (engine_name, variable_to_minimize, MSG_WARNING, 0, "Invalid starting point.");
    RESTART_ITER = 0;
  }
  else
    gsl_multimin_fminimizer_set (s, &minex_func, x, ss);

  size_t iter = 0;
  int status = GSL_CONTINUE;    /* defined in gsl_errno.h */
  double size;

  while (RESTART_ITER && status == GSL_CONTINUE && iter < MAX_ITER)
  {
    iter++;
    status = gsl_multimin_fminimizer_iterate (s);
    if (status)
      break;

    size = gsl_multimin_fminimizer_size (s);
    status = gsl_multimin_test_size (size, 1e-4);       // !! Valeur en dur ici. A voir...
  }

  if (status != GSL_SUCCESS)
    formula_on_message (engine_name, variable_to_minimize, MSG_WARNING, 0, "Did not converged to minimum.");
  else
  {
    value_t min;

    formula_get (params.minimize, &min);
    formula_on_message (engine_name, variable_to_minimize, MSG_INFO, 0, "Converged to minimum after %i iterations.",
                        iter);
    formula_on_message (engine_name, variable_to_minimize, MSG_INFO, 0, " = %g", GET_NUMBER (min));
  }

  /* free list of variables here */
  // search and destroy
  dimension_t *nd;

  for (dimension_t * d = params.degrees_of_freedom; d; d = nd)
  {
    if (status == GSL_SUCCESS)
    {
      value_t value;

      formula_get (d->variable, &value);
      formula_on_message (engine_name, d->variable->variable_name, MSG_INFO, 0, " = %g", GET_NUMBER (value));
    }
    nd = d->next;
    free (d);
  }
  params.degrees_of_freedom = 0;

  gsl_vector_free (x);
  gsl_vector_free (ss);
  gsl_multimin_fminimizer_free (s);

  engine->quiet = quiet;

  return (RESTART_ITER > 0 ? 1 : 0);
}

/*****************************************************/

static int
gsl_rootfinder (const gsl_vector * x, void *p, gsl_vector * f)
{
  rootfind_parameter_t *params = (rootfind_parameter_t *) p;
  int i;

  i = 0;
  for (dimension_t * d = params->degrees_of_freedom; d; d = d->next)
  {
    formula_t *form = d->variable;

    if (!formula_set (form, MAKE_NUMBER (x_to_f (form, gsl_vector_get (x, i)))))
      return GSL_EDOM;
    i++;
  }

  i = 0;
  for (dimension_t * d = params->nullify; d; d = d->next)
  {
    value_t value;
    formula_t *form = d->variable;

    if (!formula_get (form, &value))
      return GSL_ERANGE;
    else if (f)
      gsl_vector_set (f, i, GET_NUMBER (value));
    i++;
  }

  return GSL_SUCCESS;
}

static int
v_engine_rootfind (const char *engine_name, size_t size, const char *fargs[], const char *xargs[])
{
  const int MAX_ITER = 1000;

  int OK = 1;

  gsl_set_error_handler_off ();

  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  rootfind_parameter_t params;
  dimension_t *last;

  params.nullify = 0;
  last = 0;
  for (size_t i = 0; i < size; i++)
  {
    const char *arg;
    formula_t *f;

    arg = fargs[i];
    if (!(f = engine_identifier_get (engine_name, arg)))
    {
      formula_on_message (engine_name, arg, MSG_ERROR, 0, "Undefined variable.");
      OK = 0;
    }
    else
    {
      formula_on_message (engine_name, arg, MSG_INFO, 0, "Nullify.");
      dimension_t *d = malloc (sizeof (*d));

      CHECK_ALLOC (d);
      d->variable = f;
      d->next = 0;
      if (!params.nullify)
        params.nullify = last = d;
      else
      {
        last->next = d;
        last = d;
      }
    }
  }

  params.degrees_of_freedom = 0;
  last = 0;
  for (size_t i = 0; i < size; i++)
  {
    const char *arg;
    formula_t *f;

    arg = xargs[i];
    if (!(f = engine_identifier_get (engine_name, arg)))
    {
      formula_on_message (engine_name, arg, MSG_ERROR, 0, "Undefined variable.");
      OK = 0;
    }
    else if (f->dependencies)
    {
      formula_on_message (engine_name, f->variable_name, MSG_WARNING, 0, "'%s': Depends on other variables.",
                          f->formula);
      OK = 0;
    }
    else
    {
      formula_on_message (engine_name, arg, MSG_INFO, 0, "Root.");
      dimension_t *d = malloc (sizeof (*d));

      CHECK_ALLOC (d);
      d->variable = f;
      d->next = 0;
      if (!params.degrees_of_freedom)
        params.degrees_of_freedom = last = d;
      else
      {
        last->next = d;
        last = d;
      }
    }
  }

  int status = GSL_CONTINUE;    /* defined in gsl_errno.h */

  if (OK)
  {
    int quiet = engine->quiet;

    engine->quiet = 1;

    const gsl_multiroot_fsolver_type *T = gsl_multiroot_fsolver_hybrids;
    gsl_multiroot_fsolver *s = gsl_multiroot_fsolver_alloc (T, size);
    gsl_multiroot_function root_func;

    root_func.n = size;
    root_func.f = gsl_rootfinder;
    root_func.params = (void *) &params;

    /* Starting point */
    gsl_vector *x = gsl_vector_alloc (size);

    CHECK_ALLOC (x);
    CHECK_ALLOC (s);

    dimension_t *d = params.degrees_of_freedom;

    for (size_t i = 0; i < size; i++)
    {
      value_t value;

      if (formula_get (d->variable, &value) && IS_NUMBER (value)
          && identifier_set (engine_name, d->variable->variable_name, value))
        gsl_vector_set (x, i, f_to_x (d->variable));
      else
      {
        formula_on_message (engine_name, "Root finder", MSG_WARNING, 0, "Invalid starting point.");
        OK = 0;
      }

      d = d->next;
    }

    if (gsl_rootfinder (x, &params, 0) == GSL_NAN)
    {
      formula_on_message (engine_name, "Root finder", MSG_WARNING, 0, "Invalid starting point.");
      OK = 0;
    }
    else
      gsl_multiroot_fsolver_set (s, &root_func, x);

    size_t iter = 0;

    while (OK && status == GSL_CONTINUE && iter < MAX_ITER)
    {
      iter++;
      status = gsl_multiroot_fsolver_iterate (s);
      if (status)
        break;

      status = gsl_multiroot_test_residual (s->f, 1e-7);        // !! Valeur en dur ici. A voir...
    }

    if (status != GSL_SUCCESS)
      formula_on_message (engine_name, "Root finder", MSG_WARNING, 0, "No solution found.");
    else
      formula_on_message (engine_name, "Root finder", MSG_INFO, 0, "Solved after %i iterations.", iter);

    gsl_multiroot_fsolver_free (s);
    gsl_vector_free (x);

    engine->quiet = quiet;
  }

  /* free list of variables here */
  // search and destroy
  dimension_t *nd;

  for (dimension_t * d = params.nullify; d; d = nd)
  {
    if (status == GSL_SUCCESS)
    {
      value_t value;

      formula_get (d->variable, &value);
      formula_on_message (engine_name, d->variable->variable_name, MSG_INFO, 0, " = %g", GET_NUMBER (value));
    }
    nd = d->next;
    free (d);
  }
  params.nullify = 0;

  for (dimension_t * d = params.degrees_of_freedom; d; d = nd)
  {
    if (status == GSL_SUCCESS)
    {
      value_t value;

      formula_get (d->variable, &value);
      formula_on_message (engine_name, d->variable->variable_name, MSG_INFO, 0, " = %g", GET_NUMBER (value));
    }
    nd = d->next;
    free (d);
  }
  params.degrees_of_freedom = 0;

  return OK;
}

/**************************************************************
*                  PARSER COMMAND LINE MANAGER                *
**************************************************************/
static void
identifier_print (const char *engine_name, const char *identifier, int nbtabs, int print_values)
{
  formula_t *form = engine_identifier_get (engine_name, identifier);

  if (form)
  {
    formula_get (form, 0);      // Updates value of formula

    char base_format[50] = "";
    char message[50] = "";

    if (IS_STRING (form->value) || IS_DATE_TIME (form->value) || IS_INTEGER (form->value)
        || (!IS_NUMBER (form->min_range) && !IS_NUMBER (form->max_range)))
      /* nothing */ ;
    else if (IS_NUMBER (form->min_range) && !IS_NUMBER (form->max_range))
      snprintf (base_format, 50, " [%g;[", GET_NUMBER (form->min_range));
    else if (!IS_NUMBER (form->min_range) && IS_NUMBER (form->max_range))
      snprintf (base_format, 50, " ];%g]", GET_NUMBER (form->max_range));
    else if (IS_NUMBER (form->min_range) && IS_NUMBER (form->max_range))
      snprintf (base_format, 50, " [%g;%g]", GET_NUMBER (form->min_range), GET_NUMBER (form->max_range));

    if (!print_values)
      snprintf (message, 50, "%s%%s%%s", base_format);
    else if (!IS_VALUED (form->value))
      snprintf (message, 50, "%s%%s%%s (no value)", base_format);
    else if (IS_NUMBER (form->value))
      snprintf (message, 50, "%s%%s%%s (=%g)", base_format, GET_NUMBER (form->value));
    else if (IS_INTEGER (form->value))
      snprintf (message, 50, "%s%%s%%s (=%li)", base_format, GET_INTEGER (form->value));
    else if (IS_STRING (form->value))
      snprintf (message, 50, "%s%%s%%s (=\"%s\")", base_format, GET_STRING (form->value));
    else if (IS_DATE_TIME (form->value))
    {
      char d[200], t[200];

      if (tm_getdateintostring (GET_DATE_TIME (form->value), d, sizeof (d) / sizeof (*d)) == TM_OK &&
          tm_gettimeintostring (GET_DATE_TIME (form->value), t, sizeof (t) / sizeof (*t)) == TM_OK)
        snprintf (message, 50, "%s%%s%%s (=\'%s %s\')", base_format, d, t);
    }

    if (form->formula)
      formula_on_message (engine_name, form->variable_name, MSG_INFO, nbtabs, message, " = ", form->formula);
    else
      formula_on_message (engine_name, form->variable_name, MSG_INFO, nbtabs, message, "", "");

    for (dependency_t * dep = form->dependencies; dep; dep = dep->next)
      if (!formula_depends_on (engine_identifier_get (engine_name, dep->variable_name), form->variable_name))
        identifier_print (engine_name, dep->variable_name, nbtabs + 1, print_values);   /* recursive */
      else if (form->formula)
        formula_on_message (engine_name, form->variable_name, MSG_WARNING, 0, "'%s': Circular reference.",
                            form->formula);
  }
  else
  {
    formula_on_message (engine_name, identifier, MSG_INFO, nbtabs, " (undefined)");
  }
}

static char *
end_arg (char *l)
{
  if (!l)
    return 0;

  while (*l && !isspace (*l))   // go to end of word
    l++;

  return l;
}

static char *
next_arg (char *l)
{
  if (!l)
    return 0;

  l = end_arg (l);
  while (*l && isspace (*l))    // go to beginning of next word
    l++;

  if (*l)
    return l;
  else
    return 0;
}

inline static void
noprompt ()
{
}

inline static const char *
noecho (const char *line)
{
  return line;
}

static int
engine_read_command_line (const char *engine_name, char *command_line)
{
  if (!command_line)
    return 1;

  while (isspace (*command_line))       // Trim starting blanks
    command_line++;
  if (!*command_line)
    return 1;

  while (isspace (command_line[strlen (command_line) - 1]))     // Trim trailing blanks
    command_line[strlen (command_line) - 1] = 0;

  const char *const commands[] =
    { "INCLUDE", "SHOW", "MINIMIZE", "TREE", "COMMENT", "ALERT", "SET", "#", "SOLVE", "DELETE" };
  const int nbCommands = sizeof (commands) / sizeof (*commands);

  for (int iCommand = 0; iCommand < nbCommands; iCommand++)
  {
    const char *command = commands[iCommand];
    int len = strlen (command);

    if (!strncmp (command_line, command, len) && (isspace (command_line[len]) || command_line[len] == 0))
    {
      if (!strcmp (command, "INCLUDE"))
      {
        char *arg1 = next_arg (command_line);

        if (arg1)
        {
          engine_read_command_file (engine_name, next_arg (command_line), 0, 0);
          return 1;
        }
        else
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Missing argument file name.");
          return 0;
        }
      }
      else if (!strcmp (command, "SHOW"))
      {
        char *arg1 = next_arg (command_line);

        if (arg1)
        {
          identifier_print (engine_name, next_arg (command_line), 0, 1);
          return 1;
        }
        else
        {
          engine_tree (engine_name, 1);
          return 1;
          //formula_on_message(engine_name, command_line, MSG_ERROR, "Missing argument identifier.") ;
          //return 0 ;
        }
      }
      else if (!strcmp (command, "SET"))
      {
        engine_t *engine;

        if (!(engine = engine_get (engine_name)))
          return 0;

        char *arg1 = next_arg (command_line);
        char *arg2 = next_arg (arg1);

        if (!arg1 || !*arg1)
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Missing argument identifier.");
          return 0;
        }
        else if (!arg2)
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Missing argument value.");
          return 0;
        }

        formula_t *form = formula_allocate (0, arg2);

        form->engine = engine;
        *end_arg (arg1) = 0;
        if (formula_parse (form) && IS_VALUED (form->value))
          identifier_set (engine_name, arg1, value_dup (form->value));  //identifier_set(engine_name, arg1, value_dup(form->value)) ;
        else
          formula_on_message (engine_name, arg2, MSG_ERROR, 0, "Undefined value.");
        formula_free (form, 0); // 'form' is not needed anymore, form was not added to 'engine' (no_id=1 passed to formula_parse)

        return 1;
      }
      else if (!strcmp (command, "MINIMIZE"))
      {
        char *const variable_to_minimize = next_arg (command_line);
        const char **vargs = 0;
        char *arg = variable_to_minimize;
        size_t freedom_size = 0;

        while (next_arg (arg))
        {
          char *targ = next_arg (arg);

          *end_arg (arg) = 0;
          arg = targ;
          freedom_size++;
          vargs = (const char **) realloc (vargs, freedom_size * sizeof (const char *));
          CHECK_ALLOC (vargs);
          vargs[freedom_size - 1] = arg;
        }

        int ret = 0;

        if (!variable_to_minimize)
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Missing variable to minimize.");
          ret = 0;
        }
        else if (freedom_size == 0)
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Missing variables degrees of freedom.");
          ret = 0;
        }
        else
        {
          v_engine_minimize (engine_name, variable_to_minimize, freedom_size, vargs);
          ret = 1;
        }
        free (vargs);
        return ret;
      }
      else if (!strcmp (command, "SOLVE"))
      {
        char *arg = command_line;

        size_t size = 0;

        while ((arg = next_arg (arg)))
          size++;

        if (!size || size % 2)
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Even number of arguments expected.");
          return 0;
        }
        else
          size /= 2;

        const char **fargs = malloc (size * sizeof (*fargs));

        CHECK_ALLOC (fargs);
        const char **xargs = malloc (size * sizeof (*xargs));

        CHECK_ALLOC (xargs);

        int s = 0;

        arg = command_line;
        while (next_arg (arg))
        {
          char *targ = next_arg (arg);

          *end_arg (arg) = 0;
          arg = targ;

          if (s < size)
            fargs[s] = arg;
          else if (s < 2 * size)
            xargs[s - size] = arg;

          s++;
        }

        v_engine_rootfind (engine_name, size, fargs, xargs);

        free (fargs);
        free (xargs);

        return 1;
      }
      else if (!strcmp (command, "TREE"))
      {
        engine_tree (engine_name, 1);
        return 1;
      }
      else if (!strcmp (command, "COMMENT") || !strcmp (command, "#"))
        return 1;
      else if (!strcmp (command, "ALERT"))
      {
        char *arg1 = next_arg (command_line);
        char *arg2 = next_arg (arg1);
        char *arg3 = next_arg (arg2);
        char *arg4 = next_arg (arg3);

        if (!arg4)
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Missing arguments.");
          return 0;
        }
        *end_arg (arg1) = 0;
        *end_arg (arg2) = 0;
        *end_arg (arg3) = 0;
        char *endptr;
        value_t min = MAKE_NUMBER (strtod (arg2, &endptr));

        if ((endptr == arg2 || *endptr) && !identifier_get (engine_name, arg2, &min))
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Invalid argument %s.", arg2);
          return 0;
        }
        value_t max = MAKE_NUMBER (strtod (arg3, &endptr));

        if ((endptr == arg3 || *endptr) && !identifier_get (engine_name, arg3, &max))
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Invalid argument %s.", arg3);
          return 0;
        }
        return identifier_alert_add (engine_name, arg1, min, max, arg4);
      }
      else if (!strcmp (command, "DELETE"))
      {
        char *identifier = next_arg (command_line);

        if (engine_remove_formula (engine_name, identifier))
          return 1;
        else
        {
          formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Unknown variable %s.", identifier);
          return 0;
        }
      }
    }
  }

  char *endptr = 0;

  //TODO: accept value (GET_VALUE_FUNCTION)
  if (isid (command_line, &endptr))
  {
    if (*endptr == 0)
    {
      identifier_print (engine_name, command_line, 0, 1);
      return 1;
    }

    while (*endptr == ' ')
    {
      *endptr = 0;
      endptr++;
    }

    if (*endptr != '=')
      formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Assignation operator expected after %s.",
                          command_line);
    else
    {
      *endptr = 0;
      endptr++;
      while (*endptr == ' ')
      {
        *endptr = 0;
        endptr++;
      }
      // Here, command_line is the name of the variable, and endptr is the expression.
      if (engine_add_formula (engine_name, command_line, endptr, 0))
        return 1;
      else
        formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Could not add formula to engine.");
    }
  }
  else
    formula_on_message (engine_name, command_line, MSG_ERROR, 0, "Could not add formula to engine.");

  return 0;
}

/**************************************************************
*                      LIBRARY INTERFACE                      *
**************************************************************/
int
engine_open (const char *engine_name)
{
  if (engine_get (engine_name))
    return 0;                   // Does already exists

  for (engine_t * ptr = theEngines; ptr; ptr = ptr->next)
    if (!strcmp (ptr->name, engine_name))       // handler already registered
      return 0;

  engine_t *const pengine = malloc (sizeof (*pengine));

  CHECK_ALLOC (pengine);

  pengine->formulae = 0;
  pengine->constants = 0;
  pengine->f0args = 0;
  pengine->f1args = 0;
  pengine->f2args = 0;
  pengine->f3args = 0;
  pengine->fnargs = 0;
  pengine->next = theEngines;
  pengine->name = mystrdup (engine_name);
  pengine->automatic_calculation = 1;
  pengine->automatic_notification = 1;
  pengine->quiet = 0;
  pengine->libraries = 0;

  if (!theEngines)
    theEngines = pengine;
  else
    pengine->next = theEngines;

  // Add standard constants
  engine_add_constant (engine_name, "e", exp (1));
  engine_add_constant (engine_name, "pi", acos (-1));
  engine_add_constant (engine_name, "true", 1);
  engine_add_constant (engine_name, "false", 0);

  engine_add_f0arg (engine_name, "ran", _f_random);

  engine_add_f1arg (engine_name, "sin", sin);
  engine_add_f1arg (engine_name, "cos", cos);
  engine_add_f1arg (engine_name, "tan", tan);
  engine_add_f1arg (engine_name, "asin", asin);
  engine_add_f1arg (engine_name, "acos", acos);
  engine_add_f1arg (engine_name, "atan", atan);
  engine_add_f1arg (engine_name, "sinh", sinh);
  engine_add_f1arg (engine_name, "cosh", cosh);
  engine_add_f1arg (engine_name, "tanh", tanh);
  engine_add_f1arg (engine_name, "asinh", asinh);
  engine_add_f1arg (engine_name, "acosh", acosh);
  engine_add_f1arg (engine_name, "atanh", atanh);
  engine_add_f1arg (engine_name, "abs", fabs);
  engine_add_f1arg (engine_name, "sqrt", sqrt);
  engine_add_f1arg (engine_name, "sqr", _f_sqr);
  engine_add_f1arg (engine_name, "round", round);
  engine_add_f1arg (engine_name, "trunc", trunc);
  engine_add_f1arg (engine_name, "rint", rint);
  engine_add_f1arg (engine_name, "floor", floor);
  engine_add_f1arg (engine_name, "ceil", ceil);
  engine_add_f1arg (engine_name, "exp", exp);
  engine_add_f1arg (engine_name, "log", log);
  engine_add_f1arg (engine_name, "log10", log10);
  engine_add_f1arg (engine_name, "not", _f_not);

  engine_add_f2args (engine_name, "mod", fmod);
  engine_add_f2args (engine_name, "pow", pow);

  engine_add_fnargs (engine_name, "min", _f_min);
  engine_fnargs_set_nb_args (engine_name, "min", 1, 0);

  engine_add_fnargs (engine_name, "max", _f_max);
  engine_fnargs_set_nb_args (engine_name, "max", 1, 0);

  engine_add_fnargs (engine_name, "if", _f_iiff);
  engine_fnargs_set_nb_args (engine_name, "if", 3, 3);

  engine_add_fnargs (engine_name, "and", _f_and);
  engine_fnargs_set_nb_args (engine_name, "and", 2, 0);

  engine_add_fnargs (engine_name, "or", _f_or);
  engine_fnargs_set_nb_args (engine_name, "or", 2, 0);

  engine_add_fnargs (engine_name, "sum", _f_sum);
  engine_fnargs_set_nb_args (engine_name, "sum", 2, 0);

  engine_add_fnargs (engine_name, "to_number", _f_to_number);
  engine_fnargs_set_nb_args (engine_name, "to_number", 1, 1);

  engine_add_fnargs (engine_name, "round", _f_round_number);
  engine_fnargs_set_nb_args (engine_name, "round", 1, 1);

  engine_add_fnargs (engine_name, "diff_dates", _f_diff_dates);
  engine_fnargs_set_nb_args (engine_name, "diff_dates", 2, 3);

  engine_add_fnargs (engine_name, "add_to_date", _f_date_add);
  engine_fnargs_set_nb_args (engine_name, "add_to_date", 2, 3);

  engine_add_fnargs (engine_name, "date_attribute", _f_date_attribute);
  engine_fnargs_set_nb_args (engine_name, "date_attribute", 2, 2);

  engine_add_fnargs (engine_name, "date", _f_date);
  engine_fnargs_set_nb_args (engine_name, "date", 6, 6);

  engine_add_fnargs (engine_name, "to_string", _f_to_string);
  engine_fnargs_set_nb_args (engine_name, "to_string", 1, 5);

  engine_add_fnargs (engine_name, "now", _f_now);
  engine_fnargs_set_nb_args (engine_name, "now", 0, 1);

  engine_add_fnargs (engine_name, "today", _f_today);
  engine_fnargs_set_nb_args (engine_name, "today", 0, 1);

  // Add functions
  return 1;
}

/*********************************************************************/

void
engine_close (const char *engine_name)
{
  engine_t *previous = 0;
  engine_t *engine;

  for (engine = theEngines; engine && strcmp (engine->name, engine_name); engine = engine->next)
    previous = engine;

  if (!engine)                  // unknown engine
    return;

  while (engine->formulae)
    formula_free (engine->formulae, 0);

  free (engine->name);

  /*
     formula_t *fnext = 0 ;
     for (formula_t *expr = engine->formulae ; expr ; expr = fnext)
     {
     fnext = expr->next ;

     formula_free(expr, 0) ;
     }
   */

  constant_t *cnext = 0;

  for (constant_t * cst = engine->constants; cst; cst = cnext)
  {
    free (cst->name);
    cnext = cst->next;
    free (cst);
  }

  f0arg_t *f0next = 0;

  for (f0arg_t * f = engine->f0args; f; f = f0next)
  {
    free (f->name);
    f0next = f->next;
    free (f);
  }

  f1arg_t *f1next = 0;

  for (f1arg_t * f = engine->f1args; f; f = f1next)
  {
    free (f->name);
    f1next = f->next;
    free (f);
  }

  f2args_t *f2next = 0;

  for (f2args_t * f = engine->f2args; f; f = f2next)
  {
    free (f->name);
    f2next = f->next;
    free (f);
  }

  f3args_t *f3next = 0;

  for (f3args_t * f = engine->f3args; f; f = f3next)
  {
    free (f->name);
    f3next = f->next;
    free (f);
  }

  fnargs_t *fnnext = 0;

  for (fnargs_t * f = engine->fnargs; f; f = fnnext)
  {
    free (f->name);
    fnnext = f->next;
    free (f);
  }

  library_t *lnext = 0;

  for (library_t * l = engine->libraries; l; l = lnext)
  {
    lnext = l->next;
    dlclose (l->handler);
    free (l->filename);
    free (l);
  }

  if (previous)
    previous->next = engine->next;
  else
    theEngines = 0;

  free (engine);
}

/*********************************************************************/

void
engine_set_automatic_calculation (const char *engine_name, int mode)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return;

  engine->automatic_calculation = mode;
}

/*********************************************************************/

int
engine_add_constant (const char *engine_name, const char *constant_name, double value)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  constant_t *cst = 0;

  for (cst = engine->constants; cst && strcmp (constant_name, cst->name); cst = cst->next)
    /* nothing */ ;

  if (cst)
  {
    formula_on_message (engine_name, constant_name, MSG_ERROR, 0, "Constant already defined.");
    return 0;
  }

  cst = malloc (sizeof (*cst));
  CHECK_ALLOC (cst);

  cst->name = mystrdup (constant_name);
  cst->value = MAKE_NUMBER (value);
  //GET_NUMBER(cst->value) = value ;
  cst->next = engine->constants;
  engine->constants = cst;
  return 1;
}

/*********************************************************************/

char *
engine_next_constant (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->constants)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->constants->name);
  else
  {
    constant_t *f;

    for (f = engine->constants; f; f = f->next)
      if (!strcmp (f->name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_add_f0arg (const char *engine_name, const char *function_name, double (*value) ())
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  f0arg_t *f;

  for (f = engine->f0args; f && strcmp (function_name, f->name); f = f->next)
    /* nothing */ ;

  if (f)
  {
    formula_on_message (engine_name, function_name, MSG_ERROR, 0, "Function already defined.");
    return 0;
  }

  f = malloc (sizeof (*f));
  CHECK_ALLOC (f);

  f->name = mystrdup (function_name);
  f->value = value;
  f->next = engine->f0args;
  engine->f0args = f;
  return 1;
}

/*********************************************************************/

char *
engine_next_f0arg (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->f0args)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->f0args->name);
  else
  {
    f0arg_t *f;

    for (f = engine->f0args; f; f = f->next)
      if (!strcmp (f->name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_add_f1arg (const char *engine_name, const char *function_name, double (*value) (double))
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  f1arg_t *f;

  for (f = engine->f1args; f && strcmp (function_name, f->name); f = f->next)
    /* nothing */ ;

  if (f)
  {
    formula_on_message (engine_name, function_name, MSG_ERROR, 0, "Function already defined.");
    return 0;
  }

  f = malloc (sizeof (*f));
  CHECK_ALLOC (f);

  f->name = mystrdup (function_name);
  f->value = value;
  f->next = engine->f1args;
  engine->f1args = f;
  return 1;
}

/*********************************************************************/

char *
engine_next_f1arg (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->f1args)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->f1args->name);
  else
  {
    f1arg_t *f;

    for (f = engine->f1args; f; f = f->next)
      if (!strcmp (f->name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_add_f2args (const char *engine_name, const char *function_name, double (*value) (double, double))
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  f2args_t *f;

  for (f = engine->f2args; f && strcmp (function_name, f->name); f = f->next)
    /* nothing */ ;

  if (f)
  {
    formula_on_message (engine_name, function_name, MSG_ERROR, 0, "Function already defined.");
    return 0;
  }

  f = malloc (sizeof (*f));
  CHECK_ALLOC (f);

  f->name = mystrdup (function_name);
  f->value = value;
  f->next = engine->f2args;
  engine->f2args = f;
  return 1;
}

/*********************************************************************/

char *
engine_next_f2args (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->f2args)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->f2args->name);
  else
  {
    f2args_t *f;

    for (f = engine->f2args; f; f = f->next)
      if (!strcmp (f->name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_add_f3args (const char *engine_name, const char *function_name, double (*value) (double, double, double))
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  f3args_t *f;

  for (f = engine->f3args; f && strcmp (function_name, f->name); f = f->next)
    /* nothing */ ;

  if (f)
  {
    formula_on_message (engine_name, function_name, MSG_ERROR, 0, "Function already defined.");
    return 0;
  }

  f = malloc (sizeof (*f));
  CHECK_ALLOC (f);

  f->name = mystrdup (function_name);
  f->value = value;
  f->next = engine->f3args;
  engine->f3args = f;
  return 1;
}

/*********************************************************************/

char *
engine_next_f3args (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->f3args)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->f3args->name);
  else
  {
    f3args_t *f;

    for (f = engine->f3args; f; f = f->next)
      if (!strcmp (f->name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_add_fnargs (const char *engine_name, const char *function_name, value_t (*fct) (int, const value_t * const))
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  fnargs_t *f;

  for (f = engine->fnargs; f && strcmp (function_name, f->name); f = f->next)
    /* nothing */ ;

  if (f)
  {
    formula_on_message (engine_name, function_name, MSG_ERROR, 0, "Function already defined.");
    return 0;
  }

  f = malloc (sizeof (*f));
  CHECK_ALLOC (f);

  f->name = mystrdup (function_name);
  f->nbArgs.min = -1;
  f->nbArgs.max = -1;
  f->value = fct;
  f->next = engine->fnargs;
  engine->fnargs = f;
  return 1;
}

/*********************************************************************/

char *
engine_next_fnargs (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->fnargs)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->fnargs->name);
  else
  {
    fnargs_t *f;

    for (f = engine->fnargs; f; f = f->next)
      if (!strcmp (f->name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_fnargs_set_nb_args (const char *engine_name, const char *function_name, int nb_args_min, int nb_args_max)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  fnargs_t *f;

  for (f = engine->fnargs; f && strcmp (function_name, f->name); f = f->next)
    /* nothing */ ;

  if (!f)
  {
    formula_on_message (engine_name, function_name, MSG_ERROR, 0, "Unknown function name.");
    return 0;
  }

  if (nb_args_max && nb_args_min > nb_args_max)
  {                             // swap
    int t = nb_args_min;

    nb_args_min = nb_args_max;
    nb_args_max = t;
  }

  f->nbArgs.min = nb_args_min;
  f->nbArgs.max = nb_args_max;

  return 1;
}

/*********************************************************************/

int
engine_load_library (const char *engine_name, const char *filename)
{
  library_t **thelib;

  engine_t *engine;

  if (!engine_name)
    thelib = &theLibraries;
  else if (!(engine = engine_get (engine_name)))
    return 0;
  else
    thelib = &engine->libraries;

  void *handler = dlopen (filename, RTLD_LAZY);

  if (!handler)
  {
    formula_on_message (engine_name, filename, MSG_ERROR, 0, "Dynamic library could not be loaded.");
    return 0;
  }

  for (library_t * lib = *thelib; lib; lib = lib->next)
    if (lib->handler == handler)
    {
      dlclose (handler);
      return 1;
    }

  library_t *newlib = malloc (sizeof (*newlib));

  CHECK_ALLOC (newlib);
  newlib->next = *thelib;
  newlib->handler = handler;
  newlib->filename = strdup (filename);

  *thelib = newlib;

  return 1;
}

/*********************************************************************/

char *
engine_next_library (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  if (!engine->libraries)
  {
    free (name);
    name = 0;
  }
  else if (name == 0)
    name = strdup (engine->libraries->filename);
  else
  {
    library_t *f;

    for (f = engine->libraries; f; f = f->next)
      if (!strcmp (f->filename, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->filename);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

static int
internal_engine_add_symbol (const char *engine_name, const char *symbol_name, symbol_type type, library_t * lib)
{
  for ( /* nothing */ ; lib; lib = lib->next)
  {
    dlerror ();                 /* Clear any existing error */
    void *symbol = dlsym (lib->handler, symbol_name);

    if (!dlerror ())
    {
      switch (type)
      {

/*****************
* C99 standard leaves casting from "void *" to a function pointer undefined.
* The assignment "*(void **) (&f) = symbol ;" used below is the
* POSIX.1-2003 (Technical Corrigendum 1) workaround;
* see the Rationale for the POSIX specification of dlsym().
*
* POSIX:
* The ISO C standard does not require that pointers to functions can be cast back and forth to pointers to data. However, POSIX-
* conforming implementations are required to support this, as noted in POSIX Pointer Types.
* The result of converting a pointer to a function into a pointer to another data type (except void *) is still undefined, however.
*
* Note that compilers conforming to the ISO C standard are required to generate a warning if a conversion from a void * pointer to a 
* function pointer is attempted as in:
* 
* fptr = (int (*)(int))dlsym(handle, "my_function");
*
* POSIX Pointer Types:
* All function pointer types shall have the same representation as the type pointer to void. Conversion of a function pointer to void * 
* shall not alter the representation. A void * value resulting from such a conversion can be converted back to the original function 
* pointer type, using an explicit cast, without loss of information.
* The ISO C standard does not require this, but it is required for POSIX conformance.
*****************/

        case CONSTANT:
        {
          double *val;

          val = symbol;
          return engine_add_constant (engine_name, symbol_name, *val);
        }
          break;
        case F0ARGS:
        {
          double (*f) ();

          *(void **) (&f) = symbol;
          return engine_add_f0arg (engine_name, symbol_name, f);
        }
          break;
        case F1ARG:
        {
          double (*f) (double);

          *(void **) (&f) = symbol;
          return engine_add_f1arg (engine_name, symbol_name, f);
        }
          break;
        case F2ARGS:
        {
          double (*f) (double, double);

          *(void **) (&f) = symbol;
          return engine_add_f2args (engine_name, symbol_name, f);
        }
          break;
        case F3ARGS:
        {
          double (*f) (double, double, double);

          *(void **) (&f) = symbol;
          return engine_add_f3args (engine_name, symbol_name, f);
        }
          break;
        case FNARGS:
        {
          value_t (*f) (int, const value_t * const);
          *(void **) (&f) = symbol;
          return engine_add_fnargs (engine_name, symbol_name, f);
        }
          break;
      }
    }
  }

  return 0;
}

/*********************************************************************/

int
engine_add_symbol (const char *engine_name, const char *symbol_name, symbol_type type)
{
  engine_t *engine;

  if (!symbol_name || !(engine = engine_get (engine_name)))
    return 0;

  if (internal_engine_add_symbol (engine_name, symbol_name, type, engine->libraries)
      || internal_engine_add_symbol (engine_name, symbol_name, type, theLibraries))
  {
    engine_recalculate_all (engine_name);
    return 1;
  }

  formula_on_message (engine_name, symbol_name, MSG_ERROR, 0, "Symbol could not be added to engine.");
  return 0;
}

/*********************************************************************/

const char *
engine_add_formula (const char *engine_name, const char *variable_name, const char *formula_string, int notify_change)
{
  if (!variable_name && !formula_string)
    return 0;

  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return 0;

  formula_t *expr = formula_allocate (variable_name, formula_string);

  expr->engine = engine;
  if (notify_change && engine->automatic_notification)
  {
    engine->automatic_notification = 0;
    for (formula_t * f = engine->formulae; f; f = f->next)
      f->notify_change = 0;
    expr->notify_change = 1;
  }
  else if (engine->automatic_notification)      // && !notify_change
    expr->notify_change = 1;
  else                          // if (!engine->automatic_notification)
    expr->notify_change = notify_change;

  if (formula_parse (expr))
    return expr->variable_name;
  else
    formula_free (expr, 0);

  return 0;
}

/*********************************************************************/

int
engine_remove_formula (const char *engine_name, const char *formula)
{
  if (!formula)
    return 0;

  formula_t *form = engine_identifier_get (engine_name, formula);

  if (form)
  {
    formula_unset (form);
    formula_free (form, 0);
    return 1;
  }
  else
    return 0;
}

/*********************************************************************/

char *
engine_next_formula (const char *engine_name, char *name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)) || !engine->formulae)
    return 0;

  if (name == 0)
    name = strdup (engine->formulae->variable_name);
  else
  {
    formula_t *f;

    for (f = engine->formulae; f; f = f->next)
      if (!strcmp (f->variable_name, name))
      {
        free (name);
        if (f->next)
          name = strdup (f->next->variable_name);
        else
          name = 0;
        break;
      }
    if (!f)
      return 0;
  }

  return name;
}

/*********************************************************************/

int
engine_read_command_FILE (const char *engine_name, FILE * f, prompt_handler prompt, echo_handler echo)
{
  if (!f)
    return 0;

  if (!prompt)
    prompt = noprompt;
  if (!echo)
    echo = noecho;

  char *line = 0;

  while (prompt (), mygetline (f, &line), echo (line), line)
    if (!engine_read_command_line (engine_name, line))
      formula_on_message (engine_name, line, MSG_ERROR, 0, "Invalid command line.");

  free (line);
  return 1;
}

/*********************************************************************/

int
engine_read_command_file (const char *engine_name, const char *filename, prompt_handler prompt, echo_handler echo)
{
  FILE *f = fopen (filename, "r");

  if (!f)
  {
    formula_on_message (engine_name, filename, MSG_ERROR, 0, "Could not open file for reading.");
    return 0;
  }

  int ret = engine_read_command_FILE (engine_name, f, prompt, echo);

  fclose (f);
  return ret;
}

/*********************************************************************/

int
engine_read_file_description (const char *engine_name, const char *filename, int check_syntax_only)
{
  enum
  {
    NONE_ACTION,
    LIBRARY_ACTION,
    CONSTANT_ACTION,
    F0ARGS_ACTION,
    F1ARG_ACTION,
    F2ARGS_ACTION,
    F3ARGS_ACTION,
    FNARGS_ACTION,
    COMMAND_ACTION
  } action = NONE_ACTION;

  FILE *f = fopen (filename, "r");

  if (!f)
  {
    formula_on_message (engine_name, filename, MSG_ERROR, 0, "Could not open file for reading.");
    return 0;
  }

  char *arg = 0;
  int min = -1;
  int max = -1;

  while (fgetconf (f, 0))
  {
    if (!confvar)               // Block boundary
    {
      // Treat block
      switch (action)
      {
        case CONSTANT_ACTION:
          if (arg)
            engine_add_symbol (engine_name, arg, CONSTANT);
          break;
        case F0ARGS_ACTION:
          if (arg)
            engine_add_symbol (engine_name, arg, F0ARGS);
          break;
        case F1ARG_ACTION:
          if (arg)
            engine_add_symbol (engine_name, arg, F1ARG);
          break;
        case F2ARGS_ACTION:
          if (arg)
            engine_add_symbol (engine_name, arg, F2ARGS);
          break;
        case F3ARGS_ACTION:
          if (arg)
            engine_add_symbol (engine_name, arg, F3ARGS);
          break;
        case FNARGS_ACTION:
          if (arg && engine_add_symbol (engine_name, arg, FNARGS))
            engine_fnargs_set_nb_args (engine_name, arg, min, max);
          break;
        case COMMAND_ACTION:
          if (arg && !engine_read_command_line (engine_name, arg))
            formula_on_message (engine_name, arg, MSG_ERROR, 0, "Invalid command line.");
          break;
        case LIBRARY_ACTION:
          if (arg)
            engine_load_library (engine_name, arg);
          break;
        default:
          break;
      }

      // Prepare for next block
      free (arg);
      arg = 0;
      min = max = -1;

      if (confblk)              // Next block
      {
        if (!strcmp ("CONSTANT", confblk))
          action = CONSTANT_ACTION;
        else if (!strcmp ("F0ARGS", confblk))
          action = F0ARGS_ACTION;
        else if (!strcmp ("F1ARG", confblk))
          action = F1ARG_ACTION;
        else if (!strcmp ("F2ARGS", confblk))
          action = F2ARGS_ACTION;
        else if (!strcmp ("F3ARGS", confblk))
          action = F3ARGS_ACTION;
        else if (!strcmp ("FNARGS", confblk))
          action = FNARGS_ACTION;
        else if (!strcmp ("COMMAND", confblk))
          action = COMMAND_ACTION;
        else if (!strcmp ("LIBRARY", confblk))
          action = LIBRARY_ACTION;
        else
        {
          formula_on_message (filename, confblk, MSG_ERROR, 0, "Unrecognized block type.");
          action = NONE_ACTION;
        }
      }
    }
    else if (!check_syntax_only)
    {
      switch (action)
      {
        case FNARGS_ACTION:
          if (!strcmp ("min", confvar) && confval)
            min = atoi (confval);
          else if (!strcmp ("max", confvar) && confval)
            max = atoi (confval);
        case CONSTANT_ACTION:
        case F0ARGS_ACTION:
        case F1ARG_ACTION:
        case F2ARGS_ACTION:
        case F3ARGS_ACTION:
          if (!strcmp ("name", confvar) && confval)
          {
            free (arg);
            arg = malloc ((strlen (confval) + 1) * sizeof (*arg));
            strcpy (arg, confval);
          }
          break;
        case COMMAND_ACTION:
          if (!strcmp ("command", confvar) && !engine_read_command_line (engine_name, confval))
            formula_on_message (engine_name, confval, MSG_ERROR, 0, "Invalid command line.");
          break;
        case LIBRARY_ACTION:
          if (!strcmp ("filename", confvar))
            engine_load_library (engine_name, confval);
          break;
        default:
          formula_on_message (filename, confblk, MSG_ERROR, 0, "Unrecognized block type.");
          break;
      }
    }
  }

  free (arg);

  fclose (f);
  return 1;
}

/*********************************************************************/

void
engine_recalculate_all (const char *engine_name)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return;

  int automatic = engine->automatic_calculation;

  engine->automatic_calculation = 1;

  for (formula_t * formula = engine->formulae; formula; formula = formula->next)
  {
    //if (formula->dependencies)  // Do not recalculate constants
    ASSERT ((formula_parse (formula)), 0);
    //engine_get(engine->name, formula->variable_name) ;
  }

  engine->automatic_calculation = automatic;
}

/*********************************************************************/

int
identifier_set (const char *engine_name, const char *identifier, value_t val)
{
  formula_t *form = engine_identifier_get (engine_name, identifier);

  /*
     if (!form && engine_add_formula(engine_name, identifier, 0))
     form = engine_identifier_get(engine_name, identifier) ;
   */

  char *endptr;

  if (!form && isid (identifier, &endptr) && *endptr == 0)
  {
    engine_add_formula (engine_name, identifier, 0, 0);
    form = engine_identifier_get (engine_name, identifier);
  }
  else if (!form)
  {
    formula_on_message (engine_name, identifier, MSG_ERROR, 0, "Incorrect variable name.");
    return 0;
  }

  if (!form)
  {
    formula_on_message (engine_name, identifier, MSG_ERROR, 0, "Undefined variable name.");
    return 0;
  }

  if (!form->engine)
    return 0;

  if (form->dependencies)
  {
    formula_on_message (form->engine->name, form->variable_name, MSG_WARNING, 0,
                        "'%s': Could not be set. Depends on other variables.", form->formula);
    return 0;
  }

  free (form->formula);
  form->formula = 0;

  return formula_set (form, val);
}

/*********************************************************************/

int
identifier_get (const char *engine_name, const char *identifier, value_t * const val)
{
  formula_t *form = engine_identifier_get (engine_name, identifier);

  if (form)
    return formula_get (form, val);
  else
  {
    formula_on_message (engine_name, identifier, MSG_ERROR, 0, "Undefined variable name.");
    return 0;
  }
}

/*********************************************************************/

int
identifier_set_min_range (const char *engine_name, const char *identifier, value_t value)
{
  formula_t *form = engine_identifier_get (engine_name, identifier);

  if (form)
  {
    value_free (&form->min_range);
    form->min_range = value_dup (value);
    return 1;
  }
  else
    return 0;
}

/*********************************************************************/

int
identifier_set_max_range (const char *engine_name, const char *identifier, value_t value)
{
  formula_t *form = engine_identifier_get (engine_name, identifier);

  if (form)
  {
    value_free (&form->max_range);
    form->max_range = value_dup (value);
    return 1;
  }
  else
    return 0;
}

/*********************************************************************/

int
identifier_alert_add (const char *engine_name, const char *identifier, value_t min, value_t max, const char *message)
{
  formula_t *form = engine_identifier_get (engine_name, identifier);

  if (form)
  {
    alert_t *al = malloc (sizeof (*al));

    CHECK_ALLOC (al);

    al->message = mystrdup (message);
    al->min_value = value_dup (min);
    al->max_value = value_dup (max);
    al->next = 0;

    if (!form->alerts)
      form->alerts = al;
    else
    {
      alert_t *nal;

      for (nal = form->alerts; nal->next; nal = nal->next)
        /* nothing */ ;

      nal->next = al;
    }

    return 1;
  }
  else
  {
    formula_on_message (engine_name, identifier, MSG_ERROR, 0, "Undefined variable name.");
    return 0;
  }
}

/*********************************************************************/

void
engine_tree (const char *engine_name, int print_values)
{
  engine_t *engine;

  if (!(engine = engine_get (engine_name)))
    return;

  formula_on_message (engine_name, 0, MSG_INFO, 0, "Engine %s:", engine_name);
  for (formula_t * form = engine->formulae; form; form = form->next)
  {
    int isRoot = 1;

    for (formula_t * form2 = engine->formulae; form2 && isRoot; form2 = form2->next)
    {
      if (form2 != form)
        for (dependency_t * dep = form2->dependencies; dep && isRoot; dep = dep->next)
          if (!strcmp (dep->variable_name, form->variable_name))
            isRoot = 0;
    }
    if (isRoot || formula_depends_on (form, form->variable_name))
      identifier_print (engine_name, form->variable_name, 1, print_values);
  }
}

/*************************************************************/

int
engine_minimize (const char *engine_name, const char *variable_to_minimize, size_t freedom_size, ...)
{
  if (freedom_size > engine_size (engine_name) || freedom_size == 0)
  {
    formula_on_message (engine_name, "engine_rootfind", MSG_ERROR, 0, "Invalid size parameter.",
                        engine_size (engine_name));
    return 0;
  }

  va_list ap;

  va_start (ap, freedom_size);

  const char **vargs = malloc (freedom_size * sizeof (*vargs));

  CHECK_ALLOC (vargs);
  for (int i = 0; i < freedom_size; i++)
    vargs[i] = va_arg (ap, char *);

  int ret = v_engine_minimize (engine_name, variable_to_minimize, freedom_size, vargs);

  free (vargs);

  va_end (ap);

  return ret;
}

/*********************************************************************/

int
engine_rootfind (const char *engine_name, size_t size, ...)
{
  if (size > engine_size (engine_name) || size == 0)
  {
    formula_on_message (engine_name, "engine_rootfind", MSG_ERROR, 0, "Invalid size parameter.",
                        engine_size (engine_name));
    return 0;
  }

  va_list ap;

  va_start (ap, size);

  const char **fargs = malloc (size * sizeof (*fargs));

  CHECK_ALLOC (fargs);
  for (int i = 0; i < size; i++)
    fargs[i] = va_arg (ap, char *);

  const char **xargs = malloc (size * sizeof (*xargs));

  CHECK_ALLOC (xargs);
  for (int i = 0; i < size; i++)
    xargs[i] = va_arg (ap, char *);

  int ret = v_engine_rootfind (engine_name, size, fargs, xargs);

  free (fargs);
  free (xargs);

  va_end (ap);

  return ret;
}
