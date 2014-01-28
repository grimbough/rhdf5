#include "H5.h"

SEXP _H5open(void);
SEXP _H5close(void);
SEXP _H5garbage_collect(void);
SEXP _H5get_libversion(void);

/* herr_t H5open(void) */
SEXP _H5open(void) {
  herr_t herr = H5open();

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5close(void) */
SEXP _H5close(void) {
  herr_t herr = H5close();

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5garbage_collect(void) */
SEXP _H5garbage_collect(void) {
  herr_t herr = H5garbage_collect();

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 3));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5get_libversion( unsigned *majnum, unsigned *minnum, unsigned *relnum ) */
SEXP _H5get_libversion(void) {
  unsigned majnum;
  unsigned minnum;
  unsigned relnum;
  herr_t herr = H5get_libversion( &majnum, &minnum, &relnum );

  SEXP Rval;
  if (herr < 0) {
    error("Failed reading HDF5 library version.");
    PROTECT(Rval = allocVector(INTSXP, 1));
    INTEGER(Rval)[0] = herr;
    UNPROTECT(1);
  } else {
    PROTECT(Rval = allocVector(INTSXP, 3));
    INTEGER(Rval)[0] = majnum;
    INTEGER(Rval)[1] = minnum;
    INTEGER(Rval)[2] = relnum;

    SEXP names = PROTECT(allocVector(STRSXP,3));
    SET_STRING_ELT(names, 0, mkChar("majnum"));
    SET_STRING_ELT(names, 1, mkChar("minnum"));
    SET_STRING_ELT(names, 2, mkChar("relnum"));
    SET_NAMES(Rval, names);
    UNPROTECT(1);

    UNPROTECT(1);
  }
  return Rval;
}

