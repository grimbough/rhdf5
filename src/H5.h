#ifndef _H5_H
#define _H5_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "HandleList.h"

SEXP _H5open(void);
SEXP _H5close(void);
      	
/* _H5set_free_list_limits */
SEXP _H5garbage_collect(void);
/* _H5dont_atexit */
      	
SEXP _H5get_libversion(void);
/* _H5check_version */
/* _H5_VERSION_GE */
/* _H5_VERSION_LE */

#endif
