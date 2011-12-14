#ifndef _H5T_H
#define _H5T_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"

SEXP _H5Tcopy( SEXP _dtype_id );
SEXP _H5Tset_size( SEXP _dtype_id, SEXP _size );

#endif
