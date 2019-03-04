#ifndef _H5Z_H
#define _H5Z_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"

////////////////////////////////////////////////////
// Filter Functions
////////////////////////////////////////////////////

SEXP _H5Zfilter_avail( SEXP _filter );

#endif