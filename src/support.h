#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>

typedef enum return_as {
  AS_RAW,
  AS_STRING,
  AS_ANY
} return_as;

size_t get_key(SEXP key, const char **key_data);
size_t get_value(SEXP value, const char **value_data);
size_t get_keys(SEXP keys, const char ***key_data, size_t **key_len);
size_t get_starts_with(SEXP starts_with, const char **starts_with_data);

bool is_raw_string(const char* str, size_t len, return_as as);
SEXP raw_string_to_sexp(const char *str, size_t len, return_as as);
bool scalar_logical(SEXP x);
return_as to_return_as(SEXP x);
size_t scalar_size(SEXP x);
const char * scalar_character(SEXP x);
