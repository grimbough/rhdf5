#ifndef _h5dump_H
#define _h5dump_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"

/*################################*/
/* constants */
/*################################*/

/*################################*/
/* functions */
/*################################*/

SEXP _h5dump( SEXP _loc_id, SEXP _depth, SEXP _objecttype, SEXP _datasetinfo, SEXP _index_type, SEXP _order );

#endif
