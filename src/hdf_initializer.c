#include "common.h"

#ifdef OLD
void H5Ffinalize(SEXP file) 
{ 
    if(isFILE(file)) {
	H5Fclose(HID(file));
	R_ClearExternalPtr(VECTOR_ELT(file, 0));
    }
}

void H5Gfinalize(SEXP group)
{
    if(isGROUP(group)) {
	H5Gclose(HID(group));
	R_ClearExternalPtr(VECTOR_ELT(group, 0));
    }
}

void H5Dfinalize(SEXP dataset)
{
    if(isDATASET(dataset)) {
	H5Dclose(HID(dataset));
	R_ClearExternalPtr(VECTOR_ELT(dataset, 0));
    }
}


void H5Pfinalize(SEXP plist)
{
    if(isPLIST(plist)) {
	H5Pclose(HID(plist));
	R_ClearExternalPtr(VECTOR_ELT(plist, 0));
    }
}
#endif

void H5finalize(SEXP x)
{
    H5I_type_t ftype;
    hid_t hidx;

    if( TYPEOF(x) != EXTPTRSXP )
	error("wrong argument to H5finalize");

    hidx = (hid_t) R_ExternalPtrAddr(x);
    if( !hidx )
	return;

    ftype = H5Iget_type(hidx);

    switch(ftype) {
    case H5I_FILE:
	H5Fclose(hidx);
	break;
    case H5I_GROUP:
	H5Gclose(hidx);
	break;
    case H5I_DATASPACE:
	H5Sclose(hidx);
	break;
    case H5I_DATASET:
	H5Dclose(hidx);
	break;
    case H5I_GENPROP_LST: /* I got these from the source */
#if 0 /* gcc gives warnings for these: not part of H5I_type_t enum */
    case H5I_TEMPLATE_0:
    case H5I_TEMPLATE_1:
    case H5I_TEMPLATE_2:
    case H5I_TEMPLATE_3:
    case H5I_TEMPLATE_4:
    case H5I_TEMPLATE_5:
    case H5I_TEMPLATE_6:
    case H5I_TEMPLATE_7:
#endif
    H5Pclose(hidx);
	break;
    case H5I_ATTR:
    case H5I_BADID:
    case H5I_DATATYPE:
    case H5I_FILE_CLOSING:
#if 0 /* gcc gives warnings for this: not part of H5I_type_t enum */
    case H5I_TEMPLATE_MAX:
#endif
    case H5I_TEMPBUF:
    case H5I_REFERENCE:
    case H5I_VFL:
    case H5I_GENPROP_CLS:
    case H5I_NGROUPS:
	error("bad datatype for external pointer");
    }

    R_ClearExternalPtr(x);
    return;
}

/* we always add a hdf class to the object */
SEXP H5sexp(hid_t hid, const char* classname, void (*finalizer)(SEXP))
{
    SEXP ans, final;

    PROTECT(ans = allocVector(VECSXP,1));
    PROTECT(final = R_MakeExternalPtr((void*)hid, HDF_hid_tag, R_NilValue));
    SET_VECTOR_ELT(ans, 0, final);
    
    /* Assign a classname if specified */
    if(classname)
	addClass(ans, classname);

    /* tell them its hdf5 */
    addClass(ans, "hdf5");

    /* Assign a finalizer if specified */
    if(finalizer)
	R_RegisterCFinalizer(final, finalizer);
    
    UNPROTECT(2);
    return(ans);
}

