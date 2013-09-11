
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

  // SEXP Rval = R_NilValue;
    // vector<hid_t>::iterator it2;
    // if (invalidIDs.size() > 0) {
    //   for (it2 = invalidIDs.begin(); it2 != invalidIDs.end(); it2++) {
    // 	handleList_.erase(*it2);
    //   }
    // }
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

// void
// addFileHandle(hid_t ID, const char *name) {
//   //  HandleList::Instance().addHandle(file, ID, name, "/", file, -1, ID);
// }

// void
// addGroupHandle(hid_t ID, const char *name, hid_t loc_id) {
//   /*
//   H5Handle h;
//   h = handleInfo(file, loc_id);
//   char *groupname;
//   if (h.isvalid != 0) {
//     groupname = new char[strlen(h.group) + strlen(name) + 1];
//     strcpy(groupname, h.group);
//     strcat(groupname, name);
//     HandleList::Instance().addHandle(group, ID, name, groupname, file, loc_id, loc_id);
//   } else {
//     h = handleInfo(group, loc_id);
//     groupname = new char[strlen(h.group) + strlen(name) + 2];
//     strcpy(groupname, h.group);
//     strcat(groupname, "/");
//     strcat(groupname, name);
//     HandleList::Instance().addHandle(group, ID, name, groupname, group, h.ID, h.parentFileID);
//   }
//   */
// }

// void
// addSpaceHandle(hid_t hid, H5S_class_t type) {
//   /*
//   char *group;
//   switch(type) {
//   case H5S_SCALAR : { group = new char[11]; strcpy(group, "H5S_SCALAR"); } break;
//   case H5S_SIMPLE : { group = new char[11]; strcpy(group, "H5S_SIMPLE"); } break;
//   case H5S_NULL : { group = new char[9]; strcpy(group, "H5S_NULL"); } break;
//   case H5S_NO_CLASS : { group = new char[13]; strcpy(group, "H5S_NO_CLASS"); } break;
//   default : { group = new char[8]; strcpy(group, "unknown"); } break;
//   }
//   HandleList::Instance().addHandle(space, hid, "", group, space, -1, -1);
//   */
// }

// void
// addDatasetHandle(hid_t hid, hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id) {
//   /*
//   if (space_id <= 0) {
//     space_id = H5Dget_space( hid );
//     if (space_id > 0) {
//       H5S_class_t space_type = H5Sget_simple_extent_type(space_id);
//       addSpaceHandle(space_id, space_type);
//     }
//   }
//   if (dtype_id <= 0) {
//     dtype_id = H5Dget_type(hid);
//   }
//   H5Handle h;
//   h = handleInfo(file, loc_id);
//   if (h.isvalid != 0) {
//     HandleList::Instance().addHandle(dataset, hid, name, "", file, loc_id, loc_id, dtype_id, space_id);
//   } else {
//     h = handleInfo(group, loc_id);
//     HandleList::Instance().addHandle(dataset, hid, name, "", group, h.ID, h.parentFileID, dtype_id, space_id);
//   }
//   */
// }

// void
// addAttributeHandle(hid_t hid, hid_t obj_id, const char *name, hid_t dtype_id, hid_t space_id) {
//   /*
//   if (space_id <= 0) {
//     space_id = H5Aget_space( hid );
//     if (space_id > 0) {
//       H5S_class_t space_type = H5Sget_simple_extent_type(space_id);
//       addSpaceHandle(space_id, space_type);
//     }
//   }
//   if (dtype_id <= 0) {
//     dtype_id = H5Aget_type(hid);
//   }
//   H5Handle h;
//   h = handleInfo(file, obj_id);
//   if (h.isvalid != 0) {
//     HandleList::Instance().addHandle(attribute, hid, name, "", file, obj_id, obj_id, dtype_id, space_id);
//   } else {
//     h = handleInfo(group, obj_id);
//     if (h.isvalid != 0) {
//       HandleList::Instance().addHandle(attribute, hid, name, "", group, h.ID, h.parentFileID, dtype_id, space_id);
//     } else {
//       h = handleInfo(dataset, obj_id);
//       HandleList::Instance().addHandle(attribute, hid, name, "", dataset, h.ID, h.parentFileID, dtype_id, space_id);
//     }
//   }
//   */
// }

int
removeHandle( hid_t id ) {
  return (HandleList::Instance().removeHandle( id ) );
}

// int
// removeHandle( enum HandleType type, hid_t fid ) {
//   /*
//   if (type == dataset) {
//     H5Handle h5h = handleInfo(type, fid);
//     HandleList::Instance().removeHandle(space, h5h.spaceID);
//   }
//   HandleList::Instance().removeHandle(type, fid);
//   */
// }

// void 
// listHandles( ) {
//   /*
//   HandleList::Instance().ls();
//   */
// }

// SEXP _listHandles() {
//   /*
//   listHandles();
//   */
//   return R_NilValue;
// }

// H5Handle handleInfo(enum HandleType type, hid_t ID) {
//   H5Handle h;
//   h.isvalid = 0;
//   // = HandleList::Instance().info(type, ID);
//   return(h);
// }

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

// SEXP  handleInfoDataspace( hid_t ID) {
//   SEXP Rval = PROTECT(allocVector(VECSXP, 3));
//   ssize_t st = H5Iget_name( ID, NULL, 0 );
//   char n1[st+1];
//   H5Iget_name( ID, (char *)(&n1), st+1 );
//   SET_VECTOR_ELT(Rval, 0, mkString(n1));
//   SET_VECTOR_ELT(Rval, 1, mkString(""));
//   SET_VECTOR_ELT(Rval, 2, mkString(""));
//   SEXP names = PROTECT(allocVector(STRSXP, 2));
//   SET_STRING_ELT(names, 0, mkChar("rank"));
//   SET_STRING_ELT(names, 1, mkChar("dim"));
//   SET_STRING_ELT(names, 1, mkChar("maxdim"));
//   SET_NAMES(Rval, names);
//   UNPROTECT(2);
//   return(Rval);
// }

// SEXP  handleInfoDataset( hid_t ID) {
//   SEXP Rval = PROTECT(allocVector(VECSXP, 2));
//   ssize_t st = H5Iget_name( ID, NULL, 0 );
//   char n1[st+1];
//   H5Iget_name( ID, (char *)(&n1), st+1 );
//   SET_VECTOR_ELT(Rval, 0, mkString(n1));
//   SET_VECTOR_ELT(Rval, 1, mkString(""));
//   SEXP names = PROTECT(allocVector(STRSXP, 2));
//   SET_STRING_ELT(names, 0, mkChar("name"));
//   SET_STRING_ELT(names, 1, mkChar("filename"));
//   SET_NAMES(Rval, names);
//   UNPROTECT(2);
//   return(Rval);
// }

// SEXP  handleInfoAttr( hid_t ID) {
//   SEXP Rval = PROTECT(allocVector(VECSXP, 2));
//   ssize_t st = H5Iget_name( ID, NULL, 0 );
//   char n1[st+1];
//   H5Iget_name( ID, (char *)(&n1), st+1 );
//   SET_VECTOR_ELT(Rval, 0, mkString(n1));
//   st = H5Aget_name( ID, NULL, 0 );
//   char n1[st+1];
//   H5Iget_name( ID, (char *)(&n1), st+1 );
//   SET_VECTOR_ELT(Rval, 1, mkString(n2));


//   SET_VECTOR_ELT(Rval, 1, mkString(""));
//   SEXP names = PROTECT(allocVector(STRSXP, 2));
//   SET_STRING_ELT(names, 0, mkChar("name"));
//   SET_STRING_ELT(names, 1, mkChar("filename"));
//   SET_NAMES(Rval, names);
//   UNPROTECT(2);
//   return(Rval);
// }

SEXP _handleInfo ( SEXP _ID ) {
  int ID = INTEGER(_ID)[0];
  int isvalid = H5Iis_valid(ID);

  SEXP Rval = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(Rval,0,ScalarLogical(isvalid));
  H5I_type_t type = H5Iget_type(ID);
  SET_VECTOR_ELT(Rval,1,ScalarInteger(type));
  if (isvalid) {
    // if ((type != H5I_DATASPACE)) {
    //   ssize_t st = H5Iget_name( ID, NULL, 0 );
    //   char n1[st+1];
    //   H5Iget_name( ID, (char *)(&n1), st+1 );
    //   SET_VECTOR_ELT(Rval, 2, mkString(n1));
    // } else {
    //   SET_VECTOR_ELT(Rval, 2, mkString(""));
    // }
    // SET_VECTOR_ELT(Rval, 3, mkString(""));
    switch(type) {
    case H5I_FILE: case H5I_GROUP: case H5I_DATASET: case H5I_ATTR: {
      SET_VECTOR_ELT(Rval, 2, handleInfoName(ID));
    } break;
    // case H5I_DATATYPE: {
    //   SET_VECTOR_ELT(Rval, 2, handleInfoDatatype(ID));
    // } break;
    // case H5I_DATASPACE: {
    //   SET_VECTOR_ELT(Rval, 2, handleInfoDataspace(ID));
    // } break;
    // case H5I_DATASET: {
    //   SET_VECTOR_ELT(Rval, 2, handleInfoDataset(ID));
    // } break;
    // case H5I_ATTR: {
    //   SET_VECTOR_ELT(Rval, 2, handleInfoAttr(ID));
    // } break;
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

