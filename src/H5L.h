#ifndef _H5L_H
#define _H5L_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"

/* H5Lcreate_hard */
/* H5Lcreate_soft */
/* H5Lcreate_external */
SEXP _H5Lexists( SEXP _loc_id, SEXP _name );
/* H5Lmove */
/* H5Lcopy */
/* H5Ldelete */

SEXP _H5Lget_info( SEXP _loc_id, SEXP _name );
/* H5Lget_val */
/* H5Lunpack_elink_val  */
 
/* H5Lcreate_ud */
/* H5Lregister */
/* H5Lunregister */
/* H5Lis_registered */
      	
/* H5Literate */
/* H5Literate_by_name */
/* H5Lvisit */
/* H5Lvisit_by_name */
/* H5Lget_info_by_idx */
/* H5Lget_name_by_idx */
/* H5Lget_val_by_idx */
/* H5Ldelete_by_idx */

#endif
