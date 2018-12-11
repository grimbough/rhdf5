#include "h5testLock.h"

SEXP _h5fileLock() {
  
  SEXP Rval;
  const char *name = "/tmp/test_file";
  int fd = -1;
  int lk = -1;
  int lk2 = -1;
  int o_flags = O_RDWR | O_CREAT | O_TRUNC;
  int lock_flags = LOCK_EX;
  
  fd = HDopen(name, o_flags, 0666);
  //fd = open(name, o_flags);
  
  lk = HDflock(fd, lock_flags | LOCK_NB);
  
  Rprintf("fd: %d lk: %d\n", fd, lk);
  
  lk2 = HDflock(fd, LOCK_UN);
  
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = lk;
  UNPROTECT(1);
  
  return(Rval);
}