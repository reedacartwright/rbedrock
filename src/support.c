#include "support.h"

size_t get_data(SEXP data, const char **data_contents, const char* name);
size_t get_keys_len(SEXP keys);
void get_keys_data(size_t len, SEXP keys, const char **data, size_t *data_len);

size_t get_key(SEXP key, const char **key_data) {
  return get_data(key, key_data, "key");
}

size_t get_value(SEXP value, const char **value_data) {
  return get_data(value, value_data, "data");
}

size_t get_keys(SEXP keys, const char ***key_data, size_t **key_len) {
  size_t len = get_keys_len(keys);
  *key_data = (const char**)R_alloc(len, sizeof(const char*));
  *key_len = (size_t*)R_alloc(len, sizeof(size_t));
  get_keys_data(len, keys, *key_data, *key_len);
  return len;
}

size_t get_starts_with(SEXP starts_with, const char **starts_with_data) {
  if (starts_with == R_NilValue) {
    *starts_with_data = NULL;
    return 0;
  } else {
    return get_data(starts_with, starts_with_data, "starts_with");
  }
}

size_t get_data(SEXP data, const char **data_contents, const char* name) {
  switch (TYPEOF(data)) {
  case CHARSXP:
    *data_contents = CHAR(data);
    return length(data);
    break;
  case STRSXP:
    if (length(data) != 1) {
      Rf_error("%s must be a scalar character", name);
    }
    SEXP el = STRING_ELT(data, 0);
    *data_contents = CHAR(el);
    return length(el);
    break;
  case RAWSXP:
    *data_contents = (const char*) RAW(data);
    return length(data);
  default:
    Rf_error("Invalid data type for %s; expected string or raw", name);
  }
}

size_t get_keys_len(SEXP keys) {
  return TYPEOF(keys) == RAWSXP ? 1 : (size_t)length(keys);
}

void get_keys_data(size_t len, SEXP keys, const char **data, size_t *data_len) {
  if (TYPEOF(keys) == RAWSXP) {
    data[0] = (char*) RAW(keys);
    data_len[0] = length(keys);
  } else if (TYPEOF(keys) == STRSXP) {
    for (size_t i = 0; i < len; ++i) {
      SEXP s = STRING_ELT(keys, i);
      data[i] = CHAR(s);
      data_len[i] = length(s);
    }
  } else if (TYPEOF(keys) == VECSXP) {
    for (size_t i = 0; i < len; ++i) {
      data_len[i] = get_key(VECTOR_ELT(keys, i), data + i);
    }
  } else {
    Rf_error("Invalid type; expected a character or raw vector");
  }
}

bool is_raw_string(const char* str, size_t len, return_as as) {
  if (as == AS_RAW) {
    return true;
  } else {
    bool has_raw = memchr(str, '\0', len) != NULL;
    if (has_raw && as == AS_STRING) {
      Rf_error("Value contains embedded nul bytes; cannot return string");
    }
    return has_raw;
  }
}

// This is the same strategy as redux.
SEXP raw_string_to_sexp(const char *str, size_t len, return_as as) {
  bool is_raw = is_raw_string(str, len, as);
  SEXP ret;
  if (is_raw) {
    ret = PROTECT(allocVector(RAWSXP, len));
    memcpy(RAW(ret), str, len);
  } else {
    ret = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ret, 0, mkCharLen(str, len));
  }
  UNPROTECT(1);
  return ret;
}

// Same as from ring
bool scalar_logical(SEXP x) {
  if (TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
    int ret = INTEGER(x)[0];
    if (ret == NA_LOGICAL) {
      Rf_error("Expected a non-missing logical scalar");
    }
    return (bool)(ret);
  } else {
    Rf_error("Expected a logical scalar");
    return 0;
  }
}

return_as to_return_as(SEXP x) {
  if (x == R_NilValue) {
    return AS_ANY;
  } else if (TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
    int as_raw = INTEGER(x)[0];
    if (as_raw == NA_LOGICAL) {
      Rf_error("Expected a non-missing logical scalar (or NULL)");
    }
    return as_raw ? AS_RAW : AS_STRING;
  } else {
    Rf_error("Expected a logical scalar (or NULL)");
    return AS_ANY;
  }
}

size_t scalar_size(SEXP x) {
  int len = LENGTH(x);
  int value = 0;
  if (len == 1) {
    if (TYPEOF(x) == INTSXP) {
      value = INTEGER(x)[0];
      if (value == NA_INTEGER) {
        Rf_error("Expected a non-missing (& finite) size");
      }
    } else if (TYPEOF(x) == REALSXP) {
      double rvalue = REAL(x)[0];
      if (!R_FINITE(rvalue)) {
        Rf_error("Expected a non-missing (& finite) size");
      }
      value = (int) rvalue;
    } else {
      Rf_error("Expected a scalar size");
    }
    if (value < 0) {
      Rf_error("Expected a positive size");
    }
  } else {
    Rf_error("Expected a scalar size");
  }
  return (size_t) value;
}

const char * scalar_character(SEXP x) {
  if (LENGTH(x) == 1 && TYPEOF(x) == STRSXP) {
    return CHAR(STRING_ELT(x, 0));
  } else {
    Rf_error("Expected a scalar string");
    return NULL;
  }
}
