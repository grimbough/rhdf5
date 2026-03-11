#include "h5testLock.h"

SEXP _h5fileLock(SEXP _file_name) {
  
  const char *file_name = CHAR(STRING_ELT(_file_name, 0));
  int fd = -1;
  int lk = -1;

  /* create the temporary file */
  fd = HDopen(file_name, O_RDWR | O_CREAT | O_TRUNC, 0666);

  /* try to lock file */
  lk = HDflock(fd, LOCK_EX | LOCK_NB);
  
  /* unlock so we can remove */
  HDflock(fd, LOCK_UN);
  
  /* close */
  HDclose(fd);
  
  /* return value of lock attempt */
  return ScalarInteger(lk);
}
