#include "H5T.h"

/* hid_t H5Tcopy( hid_t dtype_id ) */
SEXP _H5Tcopy( SEXP _dtype_id ) {
  hid_t dtype_id = INTEGER(_dtype_id)[0];
  hid_t tid = H5Tcopy(dtype_id);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = tid;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Tset_size( hid_t dtype_id, size_t size )  */
SEXP _H5Tset_size( SEXP _dtype_id, SEXP _size ) {
  hid_t dtype_id = INTEGER(_dtype_id)[0];
  size_t size = INTEGER(_size)[0];
  herr_t herr = H5Tset_size(dtype_id, size);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

