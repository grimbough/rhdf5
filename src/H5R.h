#ifndef _H5R_H
#define _H5R_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "HandleList.h"

SEXP _H5Rcreate(SEXP _loc_id, SEXP name, SEXP _ref_type, SEXP _space_id);

#endif