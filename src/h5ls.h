#ifndef _h5ls_H
#define _h5ls_H

#include <stdio.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "H5G.h"
#include "H5O.h"
#include "printdatatype.h"
#include "myhdf5.h"
#include "utils.h"

SEXP _h5ls( SEXP _loc_id, SEXP _depth, SEXP _datasetinfo, SEXP _index_type, SEXP _order, SEXP _native );
SEXP _h5ls2( SEXP _loc_id, SEXP _depth, SEXP _datasetinfo, SEXP _index_type, SEXP _order, SEXP _native );

#endif
