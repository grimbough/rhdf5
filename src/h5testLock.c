#include "h5testLock.h"

SEXP _h5fileLock(SEXP _file_name) {

  SEXP Rval;
  const char *file_name = CHAR(STRING_ELT(_file_name, 0));

  /* create the temporary file */
  H5FD_t* fd = H5FDopen(file_name, H5F_ACC_CREAT, H5P_DEFAULT, /* maxaddr, max 1 MB file? */ 1000000);

  /* try to lock file */
  herr_t lk = H5FDlock(fd, /* rw = */ 1);

  /* unlock so we can remove */
  H5FDunlock(fd);

  /* close */
  H5FDclose(fd);

  /* return value of lock attempt */
  PROTECT(Rval = allocVector(LGLSXP, 1));
  LOGICAL(Rval)[0] = lk >= 0;
  UNPROTECT(1);

  return(Rval);
}
