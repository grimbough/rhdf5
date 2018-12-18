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


/*
#if defined(H5_HAVE_FLOCK)
  #define HDflock(F,L)    flock(F,L)
#elif defined(H5_HAVE_FCNTL)
  #define HDflock(F,L)    Pflock(F,L)
#else
  #define HDflock(F,L)    Nflock(F,L)
#endif
#endif */

SEXP _h5fileLock();