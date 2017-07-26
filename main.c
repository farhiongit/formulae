#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <string.h>
#include "getopts.h"
#include "formulae.h"
#include "dates.h"

/******************
*    Handlers     *
******************/
static void
prompt ()
{
  printf ("? ");
}

static const char *
echo (const char *line)
{
  if (line)
    printf ("%s\n", line);
  else
    printf ("\n");

  return line;
}

static void
print_msg (const char *engine_name, const char *formula, const char *msg, formula_msg_severity severity,
           unsigned int depth)
{
  const unsigned short TAB = 2;
  if (formula)
    printf ("%-*s(%c) [%s:%-10s] %s\n", TAB * depth, "", severity, engine_name, formula, msg);
  else
    printf ("%-*s(%c) [%s] %s\n", TAB * depth, "", severity, engine_name, msg);
}

static void
print_value (const char *engine_name, const char *variable_name, value_t value)
{
  if (IS_NUMBER (value))
    printf ("%s:%s = %g (number)\n", engine_name, variable_name, GET_NUMBER (value));
  else if (IS_INTEGER (value))
    printf ("%s:%s = %li (integer)\n", engine_name, variable_name, GET_INTEGER (value));
  else if (IS_STRING (value))
    printf ("%s:%s = \"%s\" (string)\n", engine_name, variable_name, GET_STRING (value));
  else if (IS_DATE_TIME (value))
  {
    char s[200];
    printf ("%s:%s = \'", engine_name, variable_name);
    if (tm_getdateintostring (GET_DATE_TIME (value), s, sizeof (s) / sizeof (*s)) == TM_OK)
      printf ("%s", s);
    if (tm_gettimeintostring (GET_DATE_TIME (value), s, sizeof (s) / sizeof (*s)) == TM_OK)
      printf (" %s", s);
    printf ("\' (date)\n");
  }
  else
    printf ("%s:%s undefined\n", engine_name, variable_name);
}

/******************
*  Main function  *
******************/
int
main (int argc, char *argv[])
{
  int DEBUG = 0;

  setlocale (LC_ALL, "");

  formula_changed_handler_add (print_value);
  formula_msg_handler_add (print_msg);

  const char *myEngineName = "stdin";
  if (!engine_open (myEngineName))
  {
    fprintf (stderr, "Could not create engine %s.\n", myEngineName);
    exit (1);
  }

  int letter = 0;
  const char *options = "hgf:l:c:";
  while ((letter = getopt (argc, argv, options)) >= 0)
  {
    if (letter == '?')
    {
      if (optarg)
        printf ("Invalid option -- %s. ", optarg);
      printf ("Type %s -h for options\n", argv[0]);
      exit (-1);
    }
    else if (letter == ':')
    {
      if (optarg)
        printf ("Option requires an argument -- %s. ", optarg);
      printf ("Type %s -h for options\n", argv[0]);
      exit (-1);
    }
    else if (letter == 'h')
    {
      printf ("Name:\n  %s, V%s, %s, %s\n", basename (argv[0]), FP_VERSION, FP_COPYRIGHT, FP_AUTHOR);
      printf ("\nDescription:\n  Formulae engine interpretor and processor.\n");
      printf ("\nUsage:\n  %s [-l locale] [-f program-file]\n", basename (argv[0]));
    }
    else if (letter == 'l')
    {
      if (!setlocale (LC_ALL, optarg))
        printf ("Could not set locale \"%s\".\n", optarg);
      else
        printf ("Locale \"%s\" set.\n", optarg);
    }
    else if (letter == 'f' && engine_inject_command_file (myEngineName, optarg, prompt, echo))
      printf ("File \"%s\" injected.\n", optarg);
    else if (letter == 'c' && engine_read_file_description (myEngineName, optarg, 0))
      printf ("Configuration file \"%s\" injected.\n", optarg);
    else if (letter == 'g')
      DEBUG = 1;
  }

  if (DEBUG)
  {
    printf ("Begin DEBUG ...\n");

    if (!engine_inject_command_file (myEngineName, "formulae.txt", prompt, echo))
      fprintf (stderr, "Could not inject file formulae.txt to engine %s.\n", myEngineName);

    if (!engine_add_formula (myEngineName, "z", "pi", 0))
      fprintf (stderr, "Could not add formula z=pi to engine %s.\n", myEngineName);

    identifier_set_max_range (myEngineName, "z", 2);
    identifier_set_max_range (myEngineName, "b", 2);
    identifier_set_min_range (myEngineName, "b", -20);

    if (!identifier_set (myEngineName, "z", MAKE_NUMBER (3)))
      fprintf (stderr, "Could not set identifier z in engine %s.\n", myEngineName);
    if (!identifier_set (myEngineName, "y", MAKE_NUMBER (3)))
      fprintf (stderr, "Could not set identifier y in engine %s.\n", myEngineName);
    if (!identifier_set (myEngineName, "b", MAKE_NUMBER (3)))
      fprintf (stderr, "Could not set identifier b in engine %s.\n", myEngineName);
    if (!identifier_set (myEngineName, "a", MAKE_NUMBER (3)))
      fprintf (stderr, "Could not set identifier a in engine %s.\n", myEngineName);

    value_t b;
    if (identifier_get (myEngineName, "b", &b))
      printf ("%s::%s = %g\n", myEngineName, "b", GET_NUMBER (b));

    identifier_get (myEngineName, "toto", 0);

    if (!identifier_alert_add (myEngineName, "c", 3, 3, "c is equal to 3"))
      fprintf (stderr, "Could not add alert to c of engine %s.\n", myEngineName);
    if (!identifier_alert_add (myEngineName, "c", 3, 4, "c is in [3, 4["))
      fprintf (stderr, "Could not add alert to c of engine %s.\n", myEngineName);

    engine_add_formula (myEngineName, "m", "p2 * (x - p0) * (x - p0) + p3 * (y - p1) * (y - p1) + p4 + cste", 0);
    engine_add_formula (myEngineName, "p0", "1", 0);
    engine_add_formula (myEngineName, "p1", "2", 0);
    engine_add_formula (myEngineName, "p2", "10", 0);
    engine_add_formula (myEngineName, "p3", "20", 0);
    engine_add_formula (myEngineName, "p4", "30", 0);

    engine_add_formula (myEngineName, "x", "5", 0);
    engine_add_formula (myEngineName, "cste", "10", 0);
    engine_add_formula (myEngineName, "y", "7", 0);
    engine_add_formula (myEngineName, "mm", "m*atan(m)", 0);
    engine_add_formula (myEngineName, "mmm", "mm*mm + cste", 0);

    engine_add_formula (myEngineName, "yy", " -cos(x1)*cos(x2)*exp(-sqr(x1-pi)-sqr(x2-pi))", 0);
    engine_add_formula (myEngineName, "x1", "5", 0);
    engine_add_formula (myEngineName, "x2", "5", 0);

    if (engine_minimize (myEngineName, "yy", 2, "x1", "x2"))
      engine_tree (myEngineName, 1);
    if (engine_minimize (myEngineName, "mmm", 2, "x", "y"))
      engine_tree (myEngineName, 1);

    printf ("End DEBUG.\n");
  }

  if (!engine_load_library (myEngineName, "/home/laurent/Informatique/C/Sources/formulae/dl_test.so.1.0"))
    fprintf (stderr, "Could not load dl_test.so.1.0 into engine %s.\n", myEngineName);
  else
  {
    if (!engine_add_symbol (myEngineName, "constante", CONSTANT))
      fprintf (stderr, "Could not add constante to engine %s.\n", myEngineName);
    if (!engine_add_symbol (myEngineName, "f0", F0ARGS))
      fprintf (stderr, "Could not add f0 to engine %s.\n", myEngineName);
    if (!engine_add_symbol (myEngineName, "f1", F1ARG))
      fprintf (stderr, "Could not add f1 to engine %s.\n", myEngineName);
  }

  // Read commands from terminal
  printf ("Enter command lines. Quit with Control-D.\n");
  engine_inject_command_FILE (myEngineName, stdin, prompt, 0);

  printf ("Recalculate all ...\n");
  engine_recalculate_all (myEngineName);

  char *name = 0;
  //char* name_alt = 0 ;
  while ((name = engine_next_formula (myEngineName, name)))
    printf ("Formula: %s\n", name);
  while ((name = engine_next_library (myEngineName, name)))
    printf ("Library: %s\n", name);
  while ((name = engine_next_constant (myEngineName, name)))
    printf ("Constant: %s\n", name);
  while ((name = engine_next_f0arg (myEngineName, name)))
    printf ("Function w/ no arguments: %s\n", name);
  while ((name = engine_next_f1arg (myEngineName, name)))
    printf ("Function w/ 1 argument: %s\n", name);
  while ((name = engine_next_f2args (myEngineName, name)))
    printf ("Function w/ 2 arguments: %s\n", name);
  while ((name = engine_next_f3args (myEngineName, name)))
    printf ("Function w/ 3 arguments: %s\n", name);
  while ((name = engine_next_fnargs (myEngineName, name)))
    printf ("Function w/ n arguments: %s\n", name);

  engine_tree (myEngineName, 1);

  engine_close (myEngineName);
  engine_tree (myEngineName, 1);

  formula_changed_handler_remove (0);   // remove all handlers
  formula_msg_handler_remove (0);       // remove all handlers
}
