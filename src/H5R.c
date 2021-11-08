#include "H5R.h"

SEXP _H5Rcreate(SEXP _loc_id, SEXP _name, SEXP _ref_type, SEXP _space_id) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  const char *name = CHAR(STRING_ELT(_name, 0));
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  hid_t space_id =  STRSXP_2_HID( _space_id );
  SEXP Rval;
  
  hobj_ref_t wdata[1];
  
  herr_t status = H5Rcreate (&wdata[0], loc_id, name, H5R_OBJECT, -1);
  if(status < 0) {
    error("Problem creating reference");
  }
  
  Rprintf("Pointer: %u\n", (unsigned int) wdata[0]);

  PROTECT(Rval = ScalarInteger((haddr_t) wdata[0]));
  UNPROTECT(1);
  return(Rval);
}