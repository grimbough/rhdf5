#include "common.h"
#include <time.h>

SEXP HDF_hid_tag;
SEXP HDF_SpaceDimsSymbol;
/* places to store temporary data */
SEXP HDF_workfile;
SEXP HDF_workgroup;


static void datecp(char *from, char *to)
{
    int i;

    for(i=0;i<3;i++) to[i]=from[i];
    for(i=4;i<7;i++) to[i-1]=from[i];
    for(i=8;i<10;i++) to[i-2]=from[i];
    to[8] = '\0';
}

SEXP HDF_init(void) 
{
    hid_t file, group;
    time_t t1;
    char *buf, date[20];
    
    /* Install the hid_t type tag */
    HDF_hid_tag = install("HDF_HID_TAG");
    
    /* Install the dataspace dimensions symbol */
    /* FIXME: RG, doesn't seem to get used? */
    PROTECT(HDF_SpaceDimsSymbol = allocVector(STRSXP,1));
    SET_STRING_ELT(HDF_SpaceDimsSymbol,0,mkChar("hdf5.space.dims"));
    setVar(install(".hdf5.space.dims.symbol"),
	   HDF_SpaceDimsSymbol,R_GlobalEnv);
    UNPROTECT(1);
    
    /* setup a work area for temp and intermediate files */
    file = HDF_fileopen(".hdf5.Rwork", H5P_DEFAULT);
    PROTECT(HDF_workfile = H5Fsexp(file));
    setVar(install(".hdf5.Rwork"), HDF_workfile, R_GlobalEnv);
    UNPROTECT(1);

    /* initialize a double to integer converter */

#ifdef Working
    H5Tregister(H5T_PERS_SOFT, "double2int",
             H5T_NATIVE_DOUBLE, H5T_NATIVE_INT,
             double2int);
#endif

    t1 = time(NULL);
    buf = ctime(&t1);
    datecp(buf, date);
    group = HDF_groupmkgroup(file, date);
    PROTECT(HDF_workgroup = H5Gsexp(group));
    setVar(install(".hdf5.Rwork.Current"), HDF_workgroup, R_GlobalEnv);
    UNPROTECT(1);

    return(R_NilValue);
}

SEXP HDF_is_valid(SEXP h)
{
    SEXP ans;
    
    PROTECT(ans = allocVector(LGLSXP,1));
    LOGICAL(ans)[0] = (HID(h) < 0) ? 1 : 0;
    UNPROTECT(1);
    return(ans);
}

