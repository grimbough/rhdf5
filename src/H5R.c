#include "H5R.h"

void * RAWSXP_2_REF(void *raw_ref, H5R_type_t ref_type, R_xlen_t len) {
  void *ref = R_alloc(len, 1);
  memcpy(ref, raw_ref, len);
  return(ref);
}

SEXP _H5Rcreate(SEXP _loc_id, SEXP _name, SEXP _ref_type, SEXP _space_id) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  const char *name = CHAR(STRING_ELT(_name, 0));
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  hid_t space_id = STRSXP_2_HID( _space_id );
  herr_t status;
  SEXP Rval;
  
  if(ref_type == H5R_OBJECT) {
    hobj_ref_t *ref = (hobj_ref_t *) R_alloc(sizeof(hobj_ref_t), 1);
    status = H5Rcreate (&ref[0], loc_id, name, ref_type, space_id);
    if(status < 0) {
      error("Problem creating reference");
    }
    PROTECT(Rval = allocVector(RAWSXP, sizeof(haddr_t)));
    unsigned char *Rptr = RAW(Rval);
    memcpy(Rptr, &ref[0], sizeof(haddr_t));
    
    
  } else if (ref_type == H5R_DATASET_REGION) {
    hdset_reg_ref_t *ref = (hdset_reg_ref_t *) R_alloc(sizeof(hdset_reg_ref_t), 1);
    status = H5Rcreate (&ref[0], loc_id, name, ref_type, space_id);
    if(status < 0) {
      error("Problem creating reference");
    }
    PROTECT(Rval = allocVector(RAWSXP, sizeof(hdset_reg_ref_t)));
    unsigned char *Rptr = RAW(Rval);
    memcpy(Rptr, &ref[0], sizeof(hdset_reg_ref_t));
    
  } else {
    error("Uknown reference type");
    return(R_NilValue);
  }

  /* this works for now.  haddr_t is 64-bit, so maybe we need some checks */

  UNPROTECT(1);
  return(Rval);
}

SEXP _H5Rget_obj_type(SEXP _loc_id, SEXP _ref_type, SEXP _ref) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  void *ref = RAWSXP_2_REF(RAW(_ref), ref_type, xlength(_ref));
  H5O_type_t obj_type;

  herr_t status = H5Rget_obj_type (loc_id, ref_type, ref, &obj_type);
  if(status < 0) {
    error("Problem identifying object type from reference");
    return R_NilValue;
  }

  SEXP Rval = PROTECT(allocVector(STRSXP, 1));
  switch(obj_type) {
  case H5O_TYPE_GROUP :
    Rval = mkString("GROUP");
    break;
  case H5O_TYPE_DATASET :
    Rval = mkString("DATASET");
    break;
  case H5O_TYPE_NAMED_DATATYPE :
    Rval = mkString("NAMED_DATATYPE");
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
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  void *ref = RAWSXP_2_REF(RAW(_ref), ref_type, xlength(_ref));

  hid_t obj = H5Rdereference(obj_id, H5P_DEFAULT, ref_type, ref);
  
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(obj));
  UNPROTECT(1);
  return Rval;
}

/* ssize_t H5Rget_name(hid_t loc_id, H5R_type_t ref_type, void *ref, char *name, size_t size) */
SEXP _H5Rget_name(SEXP _loc_id, SEXP _ref_type, SEXP _ref) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  void *ref = RAWSXP_2_REF(RAW(_ref), ref_type, xlength(_ref));
  
  /* first call find the number of characters in the name,
   * second call populates the buffer with the name */
  ssize_t size = H5Rget_name(loc_id, ref_type, ref, NULL, 0);
  char *buf = (char*) R_alloc(sizeof(char), size + 1);
  H5Rget_name(loc_id, ref_type, ref, buf, size + 1);
  
  SEXP Rval;
  PROTECT(Rval = mkString(buf));
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Rget_region(hid_t loc_id, H5R_type_t ref_type, void *ref) */
SEXP _H5Rget_region(SEXP _loc_id, SEXP _ref_type, SEXP _ref) {
  
  hid_t loc_id =  STRSXP_2_HID( _loc_id );
  H5R_type_t ref_type = (H5R_type_t) INTEGER(_ref_type)[0];
  void *ref = RAWSXP_2_REF(RAW(_ref), ref_type, xlength(_ref));
  
  hid_t space_id = H5Rget_region(loc_id, ref_type, ref);
  
  SEXP Rval;
  PROTECT(Rval = HID_2_STRSXP(space_id));
  UNPROTECT(1);
  return Rval;
}
