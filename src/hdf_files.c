#include "common.h"

#ifdef WIN32 
#include <sys/types.h> 
#endif 

#include <sys/stat.h> 
#include <errno.h> 


SEXP HDF_file_open(SEXP filename, SEXP access) 
{
	hid_t	file;

	if( !isString(filename) )
	  error("filename must be a character vector");
        if( length(filename) != 1)
          error("filename must be length 1");

	file = HDF_fileopen(STR(filename), HID(access));

	return H5Fsexp(file);
}

hid_t HDF_fileopen(const char *filename, hid_t createplist)
{
    hid_t file;
    struct stat buf;

    /* Check to see if the file exists first */
    if(stat(filename, &buf) == -1 && errno == ENOENT)
	file = H5Fcreate(filename, H5F_ACC_EXCL, H5P_DEFAULT,
			 H5P_DEFAULT); 
    else {
      if(!H5Fis_hdf5(filename)) 
	    error("%s is not an HDF5 file", filename);

	file = H5Fopen(filename, H5F_ACC_RDWR, createplist);
	if(file < 0)
	    error("unable to open file");
    }
    return file;
}

SEXP HDF_file_create(SEXP filename, SEXP mode, SEXP create, SEXP access)
{
	unsigned int flags=H5F_ACC_EXCL;
	hid_t	file;
	struct stat buf;

	if( !isString(filename) )
	  error("filename must be a character vector");
        if( length(filename) != 1)
          error("filename must be length 1");

	if(stat(STR(filename), &buf) == -1 && errno == ENOENT)
	    error("file: %s already exists", STR(filename));

	switch(STR(mode)[0])
	{
	case 'e':case 'E':
		flags = H5F_ACC_EXCL;
		break;
	case 't':case 'T':
		flags = H5F_ACC_TRUNC;
		break;
	default:
		error("invalid mode: %s", STR(mode));
	}

	file = H5Fcreate(STR(filename), flags, HID(create), HID(access));
	if(file < 0) 
		error ("failed to create file");
	return H5Fsexp(file);
}





