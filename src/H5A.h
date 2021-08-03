#ifndef _H5A_H
#define _H5A_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "HandleList.h"
#include "printdatatype.h"
#include "bit64conversion.h"

/*################################*/
/* functions */
/*################################*/

SEXP _H5Acreate( SEXP _loc_id, SEXP _attr_name, SEXP _type_id, SEXP _space_id );
/* H5Acreate_by_name */
SEXP _H5Aopen( SEXP _obj_id, SEXP _attr_name );
SEXP _H5Aopen_by_name( SEXP _obj_id, SEXP _obj_name, SEXP _attr_name );
SEXP _H5Aopen_by_idx( SEXP _obj_id, SEXP _obj_name, SEXP _idx_type, SEXP _order, SEXP _n );
SEXP _H5Aexists( SEXP _obj_id, SEXP _attr_name );
/* H5Aexists */
/* H5Aexists_by_name */
      	
/* H5Arename */
/* H5Arename_by_name */
SEXP _H5Awrite( SEXP _attr_id, SEXP _buf);
SEXP H5Aread_helper_INTEGER(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id, int bit64conversion);
SEXP H5Aread_helper_FLOAT(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id);
SEXP H5Aread_helper_STRING(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, hid_t dtype_id);
SEXP H5Aread_helper(hid_t attr_id, hsize_t n, SEXP Rdim, SEXP _buf, int bit64conversion );
SEXP _H5Aread( SEXP _attr_id, SEXP _buf, SEXP _bit64conversion );
SEXP _H5Aclose( SEXP _attr_id );
/* H5Aiterate */
/* H5Aiterate_by_name */
SEXP _H5Adelete( SEXP _obj_id, SEXP _attr_name );
/* H5Adelete_by_name */
/* H5Adelete_by_idx */

/* H5Aget_info */
/* H5Aget_info_by_name */
/* H5Aget_info_by_idx */
SEXP _H5Aget_name(SEXP _attr_id );
/* H5Aget_create_plist */
SEXP _H5Aget_space(SEXP _attr_id );
SEXP _H5Aget_type( SEXP _attr_id );
/* H5Aget_storage_size */
/* H5Aget_name_by_idx */


#endif
