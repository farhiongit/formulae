
/*
 * formulae: a recursive interpretor of formulae (à la Excel)
 * Copyright (C) 2017 L. Farhi (lrspam at sfr.fr)
 */
#ifndef FORMULAE_H
#define FORMULAE_H
#pragma once

//#include <strings.h>
#include <stdio.h>
#include <stdlib.h>
//#include <string.h>
#include <ctype.h>
//#include <errno.h>
//#include <math.h>
//#include <float.h>
#include <stdarg.h>
#include <time.h>

/***********************************************/
#define FP_VERSION    "1.0"
#define FP_AUTHOR     "Laurent FARHI"
#define FP_COPYRIGHT  "Copyright (c) 2010"

/***********************************************/

/***********************************************************************
* TODO:                                                                *
*                                                                      *
* - Thread-safe (mutex)                                                *
* - Divide formulae.c in several files (command, engine, commons, ...) *
* - Begin functions and type names with 'fpi_'                         *
*   (formula parser interpreter)                                       *
* - value: value does not accept constant names                        *
* - value: error message if argument is a string that is not an id     *
* - to_string: optional second argument (%g by default)                *
*                                                                      *
***********************************************************************/

/**************************************************************
*                           ENUMS                             *
**************************************************************/
typedef enum
{
  MSG_INFO = 'I',
  MSG_WARNING = 'W',
  MSG_ERROR = 'E'
} formula_msg_severity;

typedef enum
{
  CONSTANT,
  F0ARGS,
  F1ARG,
  F2ARGS,
  F3ARGS,
  FNARGS
} symbol_type;

/**************************************************************
*                          OBJECTS                            *
**************************************************************/
typedef struct
{
  enum
  {
    UNDEFINED = 0,
    NUMBER,
    INTEGER,
    STRING,
    DATE_TIME,
  } type;
  union
  {
    double number;
    long int integer;
    char *string;               // TODO: use wchar_t
    struct tm date_time;
  } value;
} value_t;

/**************************************************************
*                          TOOLS                              *
**************************************************************/

/* argument 'arg' is of type 'value' */
#define IS_VALUED(arg) ((arg).type != UNDEFINED)

#define IS_NUMBER(arg) ((arg).type == NUMBER)
#define IS_INTEGER(arg) ((arg).type == INTEGER)
#define IS_STRING(arg) ((arg).type == STRING)
#define IS_DATE_TIME(arg) ((arg).type == DATE_TIME)

#define GET_NUMBER(arg) ((arg).value.number)
#define GET_INTEGER(arg) ((arg).value.integer)
#define GET_STRING(arg) ((arg).value.string)
#define GET_DATE_TIME(arg) ((arg).value.date_time)

extern value_t MAKE_NUMBER (double v);
extern value_t MAKE_INTEGER (long int v);
extern value_t MAKE_STRING (char *v);
extern value_t MAKE_DATE_TIME (struct tm v);
extern value_t MAKE_UNDEFINED ();

/**************************************************************
*             ON FORMULA CHANGED HANDLERS MANAGER             *
**************************************************************/
typedef void (*formula_changed_handler) (const char *engine_name, const char *variable_name, value_t value);

// Adds a new handler to engine
extern void formula_changed_handler_add (formula_changed_handler);

// Removes a new handler to engine
extern void formula_changed_handler_remove (formula_changed_handler);

/**************************************************************
*                 MESSAGE HANDLERS MANAGER                    *
**************************************************************/
typedef void (*formula_msg_handler) (const char *engine_name,
                                     const char *object_name,
                                     const char *msg, formula_msg_severity severity, unsigned int depth);
// Adds a new handler to engine
extern void formula_msg_handler_add (formula_msg_handler);

// Removes a new handler to engine
extern void formula_msg_handler_remove (formula_msg_handler);

/**************************************************************
*                      ENGINES MANAGER                        *
**************************************************************/
// Creates a new engine
extern int engine_open (const char *engine_name);

// Closes an engine
extern void engine_close (const char *engine_name);

// Adds a formula to the engine, computes it (engine_formula_compute) and calls engine_formula_has_changed
extern const char *engine_add_formula (const char *engine_name, const char *variable, const char *formula,
                                       int notify_change);
extern int engine_remove_formula (const char *engine_name, const char *variable);
extern char *engine_next_formula (const char *engine_name, char *name);

extern int engine_add_constant (const char *engine_name, const char *constant_name, double v);
extern char *engine_next_constant (const char *engine_name, char *name);

extern int engine_add_f0arg (const char *engine_name, const char *function_name, double (*f) ());
extern char *engine_next_f0arg (const char *engine_name, char *name);

extern int engine_add_f1arg (const char *engine_name, const char *function_name, double (*f) (double));
extern char *engine_next_f1arg (const char *engine_name, char *name);

extern int engine_add_f2args (const char *engine_name, const char *function_name, double (*f) (double, double));
extern char *engine_next_f2args (const char *engine_name, char *name);

extern int engine_add_f3args (const char *engine_name, const char *function_name, double (*f) (double, double, double));
extern char *engine_next_f3args (const char *engine_name, char *name);

extern int engine_add_fnargs (const char *engine_name, const char *function_name,
                              value_t (*f) (int, const value_t * const));
extern char *engine_next_fnargs (const char *engine_name, char *name);
extern int engine_fnargs_set_nb_args (const char *engine_name, const char *function_name, int min_nb_args,
                                      int max_nb_args);

extern void engine_set_automatic_calculation (const char *engine_name, int automatic);
extern void engine_recalculate_all (const char *engine_name);

/**************************************************************
*                      FORMULAE MANAGER                       *
**************************************************************/
extern int identifier_get (const char *engine_name, const char *identifier, value_t * const value);
extern int identifier_set (const char *engine_name, const char *identifier, value_t value);
extern int identifier_set_min_range (const char *engine_name, const char *identifier, value_t value);
extern int identifier_set_max_range (const char *engine_name, const char *identifier, value_t value);
extern int identifier_alert_add (const char *engine_name, const char *identifier, value_t min, value_t max,
                                 const char *message);

/**************************************************************
*    OPTIMIZATION FUNCTIONS (MINIMIZER, SOLVER) USING GSL     *
**************************************************************/
extern int engine_minimize (const char *engine_name, const char *variable_to_minimize, size_t freedom_size, ...);
extern int engine_rootfind (const char *engine_name, size_t freedom_size, ...);

/**************************************************************
*                   SHARED LIBRARY LOADER                     *
**************************************************************/
extern int engine_load_library (const char *engine_name, const char *filename);
extern char *engine_next_library (const char *engine_name, char *name);

extern int engine_add_symbol (const char *engine_name, const char *symbol, symbol_type type);

/**************************************************************
*                     USER INTERFACE MANAGER                  *
**************************************************************/
typedef void (*prompt_handler) ();
typedef const char *(*echo_handler) (const char *);

extern int engine_read_command_FILE (const char *engine_name, FILE * f, prompt_handler, echo_handler);

// Calls engine_inject for each line read from file filename
extern int engine_read_command_file (const char *engine_name, const char *filename, prompt_handler, echo_handler);

extern int engine_read_file_description (const char *engine_name, const char *filename, int check_syntax_only);

extern void engine_tree (const char *engine_name, int print_values);

#endif
