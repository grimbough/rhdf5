
#include "HandleList.hpp"

extern "C" {
#include "HandleList.h"

void
addFileHandle(hid_t ID, const char *name) {
  //  HandleList::Instance().addHandle(file, ID, name, "/", file, -1, ID);
}

void
addGroupHandle(hid_t ID, const char *name, hid_t loc_id) {
  /*
  H5Handle h;
  h = handleInfo(file, loc_id);
  char *groupname;
  if (h.isvalid != 0) {
    groupname = new char[strlen(h.group) + strlen(name) + 1];
    strcpy(groupname, h.group);
    strcat(groupname, name);
    HandleList::Instance().addHandle(group, ID, name, groupname, file, loc_id, loc_id);
  } else {
    h = handleInfo(group, loc_id);
    groupname = new char[strlen(h.group) + strlen(name) + 2];
    strcpy(groupname, h.group);
    strcat(groupname, "/");
    strcat(groupname, name);
    HandleList::Instance().addHandle(group, ID, name, groupname, group, h.ID, h.parentFileID);
  }
  */
}

void
addSpaceHandle(hid_t hid, H5S_class_t type) {
  /*
  char *group;
  switch(type) {
  case H5S_SCALAR : { group = new char[11]; strcpy(group, "H5S_SCALAR"); } break;
  case H5S_SIMPLE : { group = new char[11]; strcpy(group, "H5S_SIMPLE"); } break;
  case H5S_NULL : { group = new char[9]; strcpy(group, "H5S_NULL"); } break;
  case H5S_NO_CLASS : { group = new char[13]; strcpy(group, "H5S_NO_CLASS"); } break;
  default : { group = new char[8]; strcpy(group, "unknown"); } break;
  }
  HandleList::Instance().addHandle(space, hid, "", group, space, -1, -1);
  */
}

void
addDatasetHandle(hid_t hid, hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id) {
  /*
  if (space_id <= 0) {
    space_id = H5Dget_space( hid );
    if (space_id > 0) {
      H5S_class_t space_type = H5Sget_simple_extent_type(space_id);
      addSpaceHandle(space_id, space_type);
    }
  }
  if (dtype_id <= 0) {
    dtype_id = H5Dget_type(hid);
  }
  H5Handle h;
  h = handleInfo(file, loc_id);
  if (h.isvalid != 0) {
    HandleList::Instance().addHandle(dataset, hid, name, "", file, loc_id, loc_id, dtype_id, space_id);
  } else {
    h = handleInfo(group, loc_id);
    HandleList::Instance().addHandle(dataset, hid, name, "", group, h.ID, h.parentFileID, dtype_id, space_id);
  }
  */
}

void
addAttributeHandle(hid_t hid, hid_t obj_id, const char *name, hid_t dtype_id, hid_t space_id) {
  /*
  if (space_id <= 0) {
    space_id = H5Aget_space( hid );
    if (space_id > 0) {
      H5S_class_t space_type = H5Sget_simple_extent_type(space_id);
      addSpaceHandle(space_id, space_type);
    }
  }
  if (dtype_id <= 0) {
    dtype_id = H5Aget_type(hid);
  }
  H5Handle h;
  h = handleInfo(file, obj_id);
  if (h.isvalid != 0) {
    HandleList::Instance().addHandle(attribute, hid, name, "", file, obj_id, obj_id, dtype_id, space_id);
  } else {
    h = handleInfo(group, obj_id);
    if (h.isvalid != 0) {
      HandleList::Instance().addHandle(attribute, hid, name, "", group, h.ID, h.parentFileID, dtype_id, space_id);
    } else {
      h = handleInfo(dataset, obj_id);
      HandleList::Instance().addHandle(attribute, hid, name, "", dataset, h.ID, h.parentFileID, dtype_id, space_id);
    }
  }
  */
}

int
removeHandle( enum HandleType type, hid_t fid ) {
  /*
  if (type == dataset) {
    H5Handle h5h = handleInfo(type, fid);
    HandleList::Instance().removeHandle(space, h5h.spaceID);
  }
  HandleList::Instance().removeHandle(type, fid);
  */
}

void 
listHandles( ) {
  /*
  HandleList::Instance().ls();
  */
}

SEXP _listHandles() {
  /*
  listHandles();
  */
  return R_NilValue;
}

H5Handle handleInfo(enum HandleType type, hid_t ID) {
  H5Handle h;
  h.isvalid = 0;
  // = HandleList::Instance().info(type, ID);
  return(h);
}

SEXP _handleInfo (SEXP _type, SEXP _ID) {
  // printf("typeInt = %d\n",INTEGER(_type)[0]);
  enum HandleType type = (enum HandleType)INTEGER(_type)[0];
  int ID = INTEGER(_ID)[0];

  // printf("typeEnum = %d\n",type);

  //  H5Handle h = HandleList::Instance().info(type, ID);
  SEXP Rval;
  // if (h.isvalid==0) {
    Rval = R_NilValue; 
    /*
  } else {
    Rval = PROTECT(allocVector(VECSXP, 9));

    SET_VECTOR_ELT(Rval,0,ScalarInteger(h.type));
    SET_VECTOR_ELT(Rval,1,ScalarInteger(h.ID));
    // SET_VECTOR_ELT(Rval,2,ScalarInteger(h.ID));
    // SET_VECTOR_ELT(Rval,3,ScalarInteger(h.ID));
    SET_VECTOR_ELT(Rval,2,mkString(h.name));
    SET_VECTOR_ELT(Rval,3,mkString(h.group));
    SET_VECTOR_ELT(Rval,4,ScalarInteger(h.parentType));
    SET_VECTOR_ELT(Rval,5,ScalarInteger(h.parentID));
    SET_VECTOR_ELT(Rval,6,ScalarInteger(h.parentFileID));
    SET_VECTOR_ELT(Rval,7,ScalarInteger(h.dtypeID));
    SET_VECTOR_ELT(Rval,8,ScalarInteger(h.spaceID));
    
    SEXP names = PROTECT(allocVector(STRSXP, 9));
    SET_STRING_ELT(names, 0, mkChar("type"));
    SET_STRING_ELT(names, 1, mkChar("ID"));
    SET_STRING_ELT(names, 2, mkChar("name"));
    SET_STRING_ELT(names, 3, mkChar("group"));
    SET_STRING_ELT(names, 4, mkChar("parentType"));
    SET_STRING_ELT(names, 5, mkChar("parentID"));
    SET_STRING_ELT(names, 6, mkChar("parentFileID"));
    SET_STRING_ELT(names, 7, mkChar("dtypeID"));
    SET_STRING_ELT(names, 8, mkChar("spaceID"));
    SET_NAMES(Rval, names);
    UNPROTECT(2);
  }
    */
    // if (h.isvalid != 0) {
    //   printf("name = %s\n",h.name);
    // } else {
    //   printf("file not valid!\n");
    // }
  return Rval;
}

}

