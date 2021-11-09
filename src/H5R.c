#include "H5R.h"

SEXP _H5Rcreate(SEXP _loc_id, SEXP _name, SEXP _ref_type, SEXP _space_id) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  const char *name = CHAR(STRING_ELT(_name, 0));
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  hid_t space_id =  STRSXP_2_HID( _space_id );
  SEXP Rval;
  
  hobj_ref_t *wdata = (hobj_ref_t*)R_alloc(sizeof(hobj_ref_t), 1);
  
  herr_t status = H5Rcreate (&wdata[0], loc_id, name, H5R_OBJECT, -1);
  if(status < 0) {
    error("Problem creating reference");
  }

  /* this works for now.  haddr_t is 64-bit, so maybe we need some checks */
  PROTECT(Rval = ScalarInteger((haddr_t) wdata[0]));
  UNPROTECT(1);
  return(Rval);
}

SEXP _H5Rget_obj_type(SEXP _loc_id, SEXP _ref_type, SEXP _ref) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  H5R_type_t ref_type = H5R_OBJECT;
  hobj_ref_t ref = (hobj_ref_t) INTEGER(_ref)[0];
  H5O_type_t obj_type;
  
  herr_t status = H5Rget_obj_type (loc_id, ref_type, &ref, &obj_type);
  if(status < 0) {
    error("Problem identifying object type from reference");
    return R_NilValue;
  }

  SEXP Rval = PROTECT(allocVector(STRSXP, 1));
  switch(obj_type) {
  case H5O_TYPE_GROUP :
    SET_STRING_ELT(Rval, 0, mkChar("GROUP"));
    break;
  case H5O_TYPE_DATASET :
    SET_STRING_ELT(Rval, 0, mkChar("DATASET"));
    break;
  case H5O_TYPE_NAMED_DATATYPE :
    SET_STRING_ELT(Rval, 0, mkChar("NAMED_DATATYPE"));
    break;
  default :
    Rprintf("Unknown reference type\n");
    Rval = R_NilValue;
    break;
  }
  
  UNPROTECT(1);
  return(Rval);
}

SEXP _H5Rdereference(SEXP _obj_id, SEXP _ref_type, SEXP _ref) {
  
  hid_t obj_id =  STRSXP_2_HID( _obj_id );
  H5R_type_t ref_type = H5R_OBJECT;
  hobj_ref_t ref = (hobj_ref_t) INTEGER(_ref)[0];

  hid_t obj = H5Rdereference(obj_id, H5P_DEFAULT, ref_type, &ref);
  
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(obj));
  UNPROTECT(1);
  return Rval;
}