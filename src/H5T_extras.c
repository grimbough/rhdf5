#include "H5T.h"

SEXP _h5getEnumNames( SEXP _dtype_id ) {

  hid_t dtype_id = STRSXP_2_HID( _dtype_id );
  if(H5Tget_class(dtype_id) != H5T_ENUM) {
    error("Not an H5T_ENUM datatype");
  }
  
  int nmembers = H5Tget_nmembers( dtype_id );
  SEXP Rval = PROTECT(allocVector(STRSXP, nmembers));
  for (int i=0; i<nmembers; i++) {
    char *st = H5Tget_member_name( dtype_id, i );
    SET_STRING_ELT(Rval, i, mkChar(st));
    H5free_memory(st);
  }
  
  UNPROTECT(1);
  return Rval;
}

