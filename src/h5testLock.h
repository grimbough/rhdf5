#include <fcntl.h>
#include "myhdf5.h"
#include <H5private.h>


#ifndef HDopen
  #ifdef H5_HAVE_WIN32_API
    #define HDopen(S,F,M)       _open(S,F|_O_BINARY,M)
  #else
    #define HDopen(F,...)    open(F,__VA_ARGS__)
  #endif 
#endif


SEXP _h5fileLock();