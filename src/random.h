#include <R.h>
#include <Rinternals.h>

#include <stdint.h>

SEXP mcpe_random_seed(SEXP value);
SEXP mcpe_random_state(SEXP state);

SEXP mcpe_random_get_uint(SEXP n, SEXP hi);
SEXP mcpe_random_get_int(SEXP n, SEXP lo, SEXP hi);
SEXP mcpe_random_get_double(SEXP n);
SEXP mcpe_random_get_float(SEXP n, SEXP lo, SEXP hi);
