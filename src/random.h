#include <R.h>
#include <Rinternals.h>

#include <stdint.h>

SEXP rbedrock_random_seed(SEXP r_value);
SEXP rbedrock_random_state(SEXP r_state);

SEXP rbedrock_random_get_uint(SEXP r_n, SEXP r_max);
SEXP rbedrock_random_get_int(SEXP r_n, SEXP r_min, SEXP r_max);
SEXP rbedrock_random_get_double(SEXP r_n);
SEXP rbedrock_random_get_float(SEXP r_n, SEXP r_min, SEXP r_max);

SEXP rbedrock_random_create_seed(SEXP r_x, SEXP r_z, SEXP r_a, SEXP r_b, SEXP r_salt, SEXP r_type);
