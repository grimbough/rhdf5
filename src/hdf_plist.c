#include "common.h"

#define is_class(SE,CLASS) (H5Pget_class(HID(SE) == CLASS)

SEXP HDF_plist_print(SEXP plist)
{
    if(!isDPLIST(plist)) {
	printf("Default Property List\n");
	return R_NilValue;
    }
    if(!isPLIST(plist)) {
	error("not a property list");
	return R_NilValue;
    }
    Rprintf("Property List Class: ");
    switch(H5Pget_class(HID(plist)))
    {
    case H5P_FILE_CREATE:
	printf("File Create\n");
	break;
    case H5P_FILE_ACCESS:
	printf("File Access\n");
	break;
    case H5P_DATASET_CREATE:
	printf("Dataset Create\n");
	break;
    case H5P_DATASET_XFER:
	printf("Dataset Transfer\n");
	break;
    case H5P_MOUNT:
	printf("Mount\n");
	break;
    }
    return R_NilValue;
}

SEXP HDF_plist_set_cache(SEXP plist,SEXP bytes)
{
  int mdc_nelts,rdcc_nelts;
  size_t rdcc_nbytes;
  double rdcc_w0;

  if(!isPLIST(plist)) {
    error("not a Property List");
    return R_NilValue;
  }
  H5Pget_cache(HID(plist),&mdc_nelts,&rdcc_nelts,&rdcc_nbytes,&rdcc_w0);
  rdcc_nelts = (int)((rdcc_nelts*REAL(bytes)[0])/((double)rdcc_nbytes));
  rdcc_nbytes = (size_t)(REAL(bytes)[0]);
  H5Pset_cache(HID(plist),mdc_nelts,rdcc_nelts,rdcc_nbytes,rdcc_w0);
  return R_NilValue;
}

SEXP HDF_plist_set_sizes(SEXP plist,SEXP sizes)
{
    size_t sizeof_addr;
    size_t sizeof_size;
    
    if(sizes == R_NilValue || !isNumeric(sizes) || length(sizes) != 2) {
	error("invalid size specification");
	return R_NilValue;
    }
  
    if(!isPLIST(plist)) {
	error("not a property list");
	return R_NilValue;
    }
    
    sizeof_addr = (size_t)REAL(sizes)[0];
    sizeof_size = (size_t)REAL(sizes)[1];
    
    H5Pset_sizes(HID(plist),sizeof_addr,sizeof_size);
    return(R_NilValue);
}

SEXP HDF_plist_get_sizes(SEXP plist)
{
    SEXP   ans;	
    size_t sizeof_addr;
    size_t sizeof_size;
    
    PROTECT(ans = allocVector(REALSXP,2));
    H5Pget_sizes(HID(plist),&sizeof_addr,&sizeof_size);
    REAL(ans)[0] = sizeof_addr;
    REAL(ans)[1] = sizeof_size;
    UNPROTECT(1);
    return(ans);
}
SEXP HDF_plist_default_plist(void)
{
    return H5sexp(H5P_DEFAULT, "hdf5.proplist", NULL);
}

SEXP HDF_plist_file_create(SEXP sizes,SEXP userblock) 
{
    hid_t	plist;
  
    plist = H5Pcreate(H5P_FILE_CREATE);
    return H5Psexp(plist);
}

SEXP HDF_plist_file_access(void)
{
    hid_t	plist;
  
    plist = H5Pcreate(H5P_FILE_ACCESS);
    return H5Psexp(plist);
}

SEXP HDF_plist_dataset_create(void)
{
    hid_t	plist;
  
    plist = H5Pcreate(H5P_DATASET_CREATE);
    return H5Psexp(plist);
}

SEXP HDF_plist_dataset_xfer(void)
{
    hid_t	plist;
  
    plist = H5Pcreate(H5P_DATASET_XFER);
    return H5Psexp(plist);
}

SEXP HDF_plist_mount(void)
{
    hid_t	plist;
    
    plist = H5Pcreate(H5P_MOUNT);
    return H5Psexp(plist);
}
