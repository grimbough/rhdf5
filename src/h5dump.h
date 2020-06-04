#ifndef _h5dump_H
#define _h5dump_H

#include <stdio.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5G.h"
#include "H5O.h"
#include "printdatatype.h"
#include "utils.h"

/*################################*/
/* constants */
/*################################*/

/*################################*/
/* functions */
/*################################*/

SEXP _h5dump( SEXP _loc_id, SEXP _depth, SEXP _index_type, SEXP _order );

#endif
