#define _GNU_SOURCE
#include "formulae.h"
#include <stdlib.h>
#include <string.h>

double constante = 8.62378;

double
f0 ()
{
  return drand48 ();
}

double
f1 (double k)
{
  return k * drand48 ();
}

value_t
fn (int nbArgs, const value_t * const args)
{
  if (nbArgs != 1)
    return MAKE_UNDEFINED ();

  if (IS_NUMBER (args[0]))
    return MAKE_NUMBER (2 * GET_NUMBER (args[0]));

  if (IS_STRING (args[0]))
  {
    char *str = malloc ((2 * strlen (GET_STRING (args[0])) + 1) * sizeof (*str));
    strcpy (str, GET_STRING (args[0]));
    strcat (str, GET_STRING (args[0]));
    value_t ret = MAKE_STRING (str);
    free (str);
    return ret;
  }

  return MAKE_UNDEFINED ();
}
