#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "vector.h"

const char* objtype(SEXP x) {
  return Rf_type2char(TYPEOF(x));
}

SEXP flatten_impl(SEXP x) {
  if (TYPEOF(x) != VECSXP)
    Rf_errorcall(R_NilValue, "`.x` must be a list (%s)", objtype(x));
  int m = Rf_length(x);

  // Determine output size and check type
  int n = 0;
  int has_names = 0;
  for (int j = 0; j < m; ++j) {
    SEXP x_j = VECTOR_ELT(x, j);
    if (!Rf_isVector(x_j))
      Rf_errorcall(R_NilValue, "Element %i is not a vector (%s)", j + 1, objtype(x_j));

    n += Rf_length(x_j);
    if (!has_names && !Rf_isNull(Rf_getAttrib(x_j, R_NamesSymbol))) {
      has_names = 1;
    }
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));
  if (has_names)
    Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(1);

  int i = 0;
  for (int j = 0; j < m; ++j) {
    SEXP x_j = VECTOR_ELT(x, j);
    int n_j = Rf_length(x_j);

    SEXP names_j = Rf_getAttrib(x_j, R_NamesSymbol);
    int has_names_j = !Rf_isNull(names_j);

    for (int k = 0; k < n_j; ++k, ++i) {
      switch(TYPEOF(x_j)) {
      case LGLSXP:   SET_VECTOR_ELT(out, i, Rf_ScalarLogical(LOGICAL(x_j)[k])); break;
      case INTSXP:   SET_VECTOR_ELT(out, i, Rf_ScalarInteger(INTEGER(x_j)[k])); break;
      case REALSXP:  SET_VECTOR_ELT(out, i, Rf_ScalarReal(REAL(x_j)[k])); break;
      case STRSXP:   SET_VECTOR_ELT(out, i, Rf_ScalarString(STRING_ELT(x_j, k))); break;
      case VECSXP:   SET_VECTOR_ELT(out, i, VECTOR_ELT(x_j, k)); break;
      default:
        Rf_errorcall(R_NilValue, "Unsupported type at element %i (%s)", j + 1, objtype(x_j));
      }
      if (has_names)
        SET_STRING_ELT(names, i, has_names_j ? STRING_ELT(names_j, k) : Rf_mkChar(""));
      if (i % 1000 == 0)
        R_CheckUserInterrupt();
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP vflatten_impl(SEXP x, SEXP type_) {
  if (TYPEOF(x) != VECSXP)
    Rf_errorcall(R_NilValue, "`.x` must be a list (%s)", objtype(x));
  int m = Rf_length(x);

  SEXPTYPE type = Rf_str2type(CHAR(Rf_asChar(type_)));

  // Determine output size and type
  int n = 0;
  int has_names = 0;
  for (int j = 0; j < m; ++j) {
    SEXP x_j = VECTOR_ELT(x, j);

    ensure_can_coerce(TYPEOF(x_j), type, j);

    n += Rf_length(x_j);
    if (!has_names && !Rf_isNull(Rf_getAttrib(x_j, R_NamesSymbol))) {
      has_names = 1;
    }
  }

  SEXP out = PROTECT(Rf_allocVector(type, n));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));
  if (has_names)
    Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(1);

  int i = 0;
  for (int j = 0; j < m; ++j) {
    SEXP x_j = VECTOR_ELT(x, j);
    int n_j = Rf_length(x_j);

    SEXP names_j = Rf_getAttrib(x_j, R_NamesSymbol);
    int has_names_j = !Rf_isNull(names_j);

    for (int k = 0; k < n_j; ++k, ++i) {
      set_vector_value(out, i, x_j, k);

      if (has_names)
        SET_STRING_ELT(names, i, has_names_j ? STRING_ELT(names_j, k) : Rf_mkChar(""));
      if (i % 1000 == 0)
        R_CheckUserInterrupt();
    }
  }

  UNPROTECT(1);
  return out;
}
