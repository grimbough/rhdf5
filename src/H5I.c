#include "H5I.h"

// H5I_type_t H5Iget_type( hid_t obj_id )
SEXP _H5Iget_type( SEXP _obj_id ) {
  hid_t obj_id = INTEGER(_obj_id)[0];
  H5I_type_t t = H5Iget_type(obj_id);
  SEXP Rval = ScalarInteger(t);
  return(Rval);
}

