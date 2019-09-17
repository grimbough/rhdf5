#ifndef _h5ls_H
#define _h5ls_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"

SEXP _h5ls( SEXP _loc_id, SEXP _depth, SEXP _datasetinfo, SEXP _index_type, SEXP _order, SEXP _native );

#endif
