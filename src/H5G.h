#ifndef _H5G_H
#define _H5G_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"
#include "HandleList.h"

SEXP _H5Gcreate( SEXP _loc_id, SEXP _name );
SEXP _H5Gcreate_anon( SEXP _loc_id );
SEXP _H5Gopen( SEXP _loc_id, SEXP _name );
SEXP _H5Gclose( SEXP _group_id );
      	
SEXP _H5Gget_info( SEXP _group_id );
SEXP _H5Gget_info_by_name( SEXP _loc_id, SEXP _group_name );
      	
/* H5Gget_create_plist */
SEXP _H5Gget_info_by_idx( SEXP _loc_id, SEXP _group_name,  SEXP _index_type, SEXP _order, SEXP _n);

#endif
