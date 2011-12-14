#ifndef _H5O_H
#define _H5O_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"

/* H5Oopen */
/* H5Olink */
/* H5Oclose */
/* H5Ocopy */
/* H5Ovisit */
/* H5Ovisit_by_name */
      	
/* H5Oset_comment */
/* H5Oset_comment_by_name */
/* H5Oget_comment */
/* H5Oget_comment_by_name */
SEXP _H5Oget_info( SEXP _object_id );
SEXP _H5Oget_info_by_name( SEXP _loc_id, SEXP _object_name );
/* H5Oget_info_by_idx */
      	
/* H5Oopen_by_idx */
/* H5Oopen_by_addr  */
 
/* H5Oincr_refcount */
/* H5Odecr_refcount */

#endif
