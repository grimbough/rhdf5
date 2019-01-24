#ifndef _H5WRITEDATAFRAME_H
#define _H5WRITEDATAFRAME_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "HandleList.h"
#include "printdatatype.h"

SEXP _h5writeDataFrame(SEXP _obj, SEXP _dset_id);
SEXP _h5createDataFrame(SEXP _obj, SEXP _loc_id, SEXP _name, SEXP _level, SEXP _chunk);

#endif
