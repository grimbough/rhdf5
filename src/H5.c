#include "H5.h"

SEXP _H5open(void);
SEXP _H5close(void);
SEXP _H5garbage_collect(void);
SEXP _H5get_libversion(void);

/* herr_t H5open(void) */
SEXP _H5open(void) {
  herr_t herr = H5open();

  return ScalarInteger(herr);
}

/* herr_t H5close(void) */
SEXP _H5close(void) {
  herr_t herr = H5close();

  return ScalarInteger(herr);
}

/* herr_t H5garbage_collect(void) */
SEXP _H5garbage_collect(void) {
  herr_t herr = H5garbage_collect();

  SEXP Rval = allocVector(INTSXP, 3);
  INTEGER(Rval)[0] = herr;
  return Rval;
}

/* herr_t H5get_libversion( unsigned *majnum, unsigned *minnum, unsigned *relnum ) */
SEXP _H5get_libversion(void) {
  unsigned majnum;
  unsigned minnum;
  unsigned relnum;
  herr_t herr = H5get_libversion( &majnum, &minnum, &relnum );

  if (herr < 0) {
    error("Failed reading HDF5 library version.");
    return ScalarInteger(herr);
  }
  
  const char *nms[] = {"majnum", "minnum", "relnum", ""};
  SEXP Rval = PROTECT(Rf_mkNamed(INTSXP, nms));
  INTEGER(Rval)[0] = majnum;
  INTEGER(Rval)[1] = minnum;
  INTEGER(Rval)[2] = relnum;

  UNPROTECT(1);

  return Rval;
}

