
#include "HandleListcpp.h"

extern "C" {
#include "HandleList.h"

void
addHandle( hid_t id ) {
  HandleList::Instance().addHandle( id );
}

SEXP _h5listIdentifier( ) {
  std::vector<hid_t> validIDs;
  HandleList::Instance().validIdentifier( validIDs );

  SEXP Rval = PROTECT(allocVector(VECSXP, 2));

  SEXP type = PROTECT(allocVector(INTSXP, validIDs.size()));
  SEXP name = PROTECT(allocVector(STRSXP, validIDs.size()));

  if (validIDs.size() > 0) {
    std::vector<hid_t>::iterator it;
    int i=0;
    H5I_type_t t;
    ssize_t st;
    for (it = validIDs.begin(), i=0; it != validIDs.end(); it++, i++) {
      t = H5Iget_type(*it);
      INTEGER(type)[i] = t;
      if ((t == H5I_FILE) || (t == H5I_GROUP) || (t == H5I_DATASET) || (t == H5I_ATTR)) {
	st = H5Iget_name( *it, NULL, 0 );
	st = st+1;
	char n1[st];
	H5Iget_name( *it, (char *)(&n1), st );
	SET_STRING_ELT(name, i, mkChar(n1));
      } else {
	SET_STRING_ELT(name, i, mkChar(""));
      }
    }
  }

  SET_VECTOR_ELT(Rval,0,type);
  SET_VECTOR_ELT(Rval,1,name);
    
  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("type"));
  SET_STRING_ELT(names, 1, mkChar("name"));
  SET_NAMES(Rval, names);
  UNPROTECT(4);

  return(Rval);
}

SEXP _h5validObjects( ) {
  std::vector<hid_t> validIDs;
  HandleList::Instance().validIdentifier( validIDs );

  SEXP Rval = PROTECT(allocVector(INTSXP, validIDs.size()));
  if (validIDs.size() > 0) {
    std::vector<hid_t>::iterator it;
    int i=0;
    for (it = validIDs.begin(), i=0; it != validIDs.end(); it++, i++) {
      INTEGER(Rval)[i] = *it;
    }
  }
  UNPROTECT(1);

  return(Rval);
}


void
removeHandle( hid_t id ) {
  HandleList::Instance().removeHandle( id );
}

SEXP  handleInfoName( hid_t ID) {
  SEXP Rval = PROTECT(allocVector(VECSXP, 2));
  ssize_t st = H5Iget_name( ID, NULL, 0 );
  char n1[st+1];
  H5Iget_name( ID, (char *)(&n1), st+1 );
  SET_VECTOR_ELT(Rval, 0, mkString(n1));
  SET_VECTOR_ELT(Rval, 1, mkString(""));
  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("name"));
  SET_STRING_ELT(names, 1, mkChar("filename"));
  SET_NAMES(Rval, names);
  UNPROTECT(2);
  return(Rval);
}

SEXP _handleInfo ( SEXP _ID ) {
  int ID = INTEGER(_ID)[0];
  int isvalid = H5Iis_valid(ID);

  SEXP Rval = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(Rval,0,ScalarLogical(isvalid));
  H5I_type_t type = H5Iget_type(ID);
  SET_VECTOR_ELT(Rval,1,ScalarInteger(type));
  if (isvalid) {
    switch(type) {
    case H5I_FILE: case H5I_GROUP: case H5I_DATASET: case H5I_ATTR: {
      SET_VECTOR_ELT(Rval, 2, handleInfoName(ID));
    } break;
    default: {
      SET_VECTOR_ELT(Rval, 2, R_NilValue);
    }
    }
  } else {
    SET_VECTOR_ELT(Rval, 2, mkString(""));
  }
  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("isvalid"));
  SET_STRING_ELT(names, 1, mkChar("type"));
  SET_STRING_ELT(names, 2, mkChar("info"));
  SET_NAMES(Rval, names);
  UNPROTECT(2);
  return Rval;
}

}

