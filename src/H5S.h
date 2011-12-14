#ifndef _H5S_H
#define _H5S_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"
#include "HandleList.h"

SEXP _H5Screate( SEXP _type );
SEXP _H5Scopy( SEXP _space_id );
SEXP _H5Sclose( SEXP _space_id );
/* H5Sdecode */
/* H5Sencode */
SEXP _H5Screate_simple( SEXP _dims, SEXP _maxdims );
SEXP _H5Sis_simple( SEXP _space_id );
/* H5Soffset_simple */
SEXP _H5Sget_simple_extent_dims( SEXP _space_id );
/* H5Sget_simple_extent_ndims */
      	
/* H5Sget_simple_extent_npoints */
/* H5Sget_simple_extent_type */
/* H5Sextent_copy */
/* H5Sextent_equal */
/* H5Sset_extent_simple */
/* H5Sset_extent_none */
/* H5Sget_select_type */
/* H5Sget_select_npoints */
/* H5Sget_select_hyper_nblocks */
/* H5Sget_select_hyper_blocklist */
      	
/* H5Sget_select_elem_npoints */
/* H5Sget_select_elem_pointlist */
/* H5Sget_select_bounds */
/* H5Sselect_elements */
/* H5Sselect_all */
/* H5Sselect_none */
/* H5Sselect_valid */
SEXP _H5Sselect_hyperslab( SEXP _space_id, SEXP _op, SEXP _start, SEXP _stride, SEXP _count, SEXP _block );
SEXP _H5Sselect_index( SEXP _space_id, SEXP _start, SEXP _count);


#endif
