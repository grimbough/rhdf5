#include "common.h"

typedef struct {
  const char* name;
  hid_t hid;
} typeEntry;

#define R(S,H) if(stricmp(STR(name),S) == 0) return H;

hid_t hdf_datatype_resolve(SEXP name) 
{
  R("char", H5T_NATIVE_CHAR);
  R("uchar", H5T_NATIVE_UCHAR);
  R("short", H5T_NATIVE_SHORT);
  R("ushort", H5T_NATIVE_USHORT);
  R("int", H5T_NATIVE_INT);
  R("uint", H5T_NATIVE_UINT);
  R("long", H5T_NATIVE_LONG);
  R("ulong", H5T_NATIVE_ULONG);
  R("float", H5T_NATIVE_FLOAT);
  R("double", H5T_NATIVE_DOUBLE);
  return(-1);
}

hid_t Rtype2HDFtype(SEXPTYPE Rtype)
{
    switch(Rtype) {
    case INTSXP:
    case LGLSXP:
	return H5T_NATIVE_INT;
    case REALSXP:
	return H5T_NATIVE_DOUBLE;
    case STRSXP:
	error("not implemented");
    default:
	error("bad type conversion");
    }
    return H5T_NATIVE_DOUBLE; /* not reached */
}

SEXPTYPE HDFclass2Rtype(hid_t HDFtype)
{
    switch(HDFtype) {
    case H5T_INTEGER:
	return INTSXP;
    case H5T_FLOAT:
	return REALSXP;
    default:
	error("not implemented yet");
    }
    return REALSXP; /* not reached */
}


