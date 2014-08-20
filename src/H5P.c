#include "H5P.h"

////////////////////////////////////////////////////
// General Property List Operations
////////////////////////////////////////////////////

/* hid_t H5Pcreate( hid_t cls_id ) */
SEXP _H5Pcreate( SEXP _cls_id ) {
  hid_t cls_id =  INTEGER(_cls_id)[0];
  hid_t hid = H5Pcreate( cls_id );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = hid;
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Pget_class(hid_t plist ) */
SEXP _H5Pget_class( SEXP _plist ) {
  hid_t plist =  INTEGER(_plist)[0];
  hid_t hid = H5Pget_class( plist );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = hid;
  UNPROTECT(1);
  return Rval;
}

/* hid_t H5Pcopy(hid_t plist ) */
SEXP _H5Pcopy( SEXP _plist ) {
  hid_t plist =  INTEGER(_plist)[0];
  hid_t hid = H5Pcopy( plist );
  addHandle(hid);

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = hid;
  UNPROTECT(1);
  return Rval;
}

/* herr_t H5Pclose(hid_t plist ) */
SEXP _H5Pclose( SEXP _plist ) {
  hid_t plist =  INTEGER(_plist)[0];
  herr_t herr = H5Pclose( plist );
  if (herr == 0) {
    removeHandle(plist);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

////////////////////////////////////////////////////
// File Access Properties
////////////////////////////////////////////////////

/* H5Pset_driver  */
/* H5Pget_driver  */
/* H5Pget_driver_info  */
/* H5Pset_fclose_degree  */
/* H5Pget_fclose_degree      */
/* H5Pset_fapl_core  */
/* H5Pget_fapl_core  */
/* H5Pset_core_write_tracking  */
/* H5Pget_core_write_tracking  */
/* H5Pset_fapl_direct  */
/* H5Pget_fapl_direct  */
/* H5Pset_fapl_family  */
/* H5Pget_fapl_family  */
/* H5Pset_family_offset  */
/* H5Pget_family_offset  */
/* H5Pset_fapl_log */
/* mpi: H5Pset_fapl_mpio */
/* mpi: H5Pget_fapl_mpio */
/* mpi: H5Pset_fapl_mpiposix */
/* mpi: H5Pget_fapl_mpiposix */
/* H5Pset_fapl_multi  */
/* H5Pget_fapl_multi  */
/* H5Pset_multi_type  */
/* H5Pget_multi_type  */
/* H5Pset_fapl_split  */
/* H5Pset_fapl_sec2  */
/* H5Pset_fapl_stdio  */
/* H5Pset_fapl_windows  */
/* H5Pset_file_image  */
/* H5Pget_file_image  */
/* H5Pset_file_image_callbacks  */
/* H5Pget_file_image_callbacks      */
/* H5Pset_meta_block_size  */
/* H5Pget_meta_block_size */
/* H5Pset_sieve_buf_size  */
/* H5Pget_sieve_buf_size  */
/* H5Pset_alignment  */
/* H5Pget_alignment  */
/* H5Pset_cache  */
/* H5Pget_cache  */
/* H5Pset_elink_file_cache_size  */
/* H5Pget_elink_file_cache_size  */
/* H5Pset_mdc_config  */
/* H5Pget_mdc_config  */
/* H5Pset_gc_references  */
/* H5Pget_gc_references  */
/* H5Pset_small_data_block_size  */
/* H5Pget_small_data_block_size  */
/* H5Pset_libver_bounds  */
/* H5Pget_libver_bounds */

////////////////////////////////////////////////////
// Group Creation Properties
////////////////////////////////////////////////////

/* H5Pset_local_heap_size_hint  */
/* H5Pget_local_heap_size_hint  */
/* H5Pset_link_creation_order */
/* H5Pget_link_creation_order      */
/* H5Pset_est_link_info  */
/* H5Pget_est_link_info */
/* H5Pset_link_phase_change  */
/* H5Pget_link_phase_change */

////////////////////////////////////////////////////
// Link Creation Properties
////////////////////////////////////////////////////

/* H5Pset_char_encoding */
/* H5Pget_char_encoding */
/* H5Pset_create_intermediate_group */
/* H5Pget_create_intermediate_group */

////////////////////////////////////////////////////
// Link Access Properties
////////////////////////////////////////////////////

/* H5Pset_nlinks  */
/* H5Pget_nlinks */
/* H5Pset_elink_cb */
/* H5Pget_elink_cb */
/* H5Pset_elink_prefix */
/* H5Pget_elink_prefix */
/* H5Pset_elink_fapl */
/* H5Pget_elink_fapl */
/* H5Pset_elink_acc_flags */
/* H5Pget_elink_acc_flags */

////////////////////////////////////////////////////
// Dataset Creation Properties
////////////////////////////////////////////////////

/* H5Pset_layout */
/* H5Pget_layout  */
/* H5Pset_chunk  */
/* H5Pget_chunk  */
/* H5Pset_deflate  */
/* H5Pset_fill_value  */
/* H5Pget_fill_value  */
/* H5Pfill_value_defined  */
/* H5Pset_fill_time  */
/* H5Pget_fill_time */
/* H5Pset_alloc_time  */
/* H5Pget_alloc_time  */
/* H5Pset_filter  */
/* H5Pall_filters_avail  */
/* H5Pget_nfilters  */
/* H5Pget_filter  */
/* H5Pget_filter1  *  */
/* H5Pget_filter2  */
/* H5Pget_filter_by_id  */
/* H5Pget_filter_by_id1  *  */
/* H5Pget_filter_by_id2 */
/* H5Pmodify_filter  */
/* H5Premove_filter  */
/* H5Pset_fletcher32  */
/* H5Pset_nbit  */
/* H5Pset_scaleoffset  */
/* H5Pset_shuffle  */
/* H5Pset_szip  */
/* H5Pset_external  */
/* H5Pget_external_count  */
/* H5Pget_external */

////////////////////////////////////////////////////
// Dataset Access Properties
////////////////////////////////////////////////////

/* H5Pset_chunk_cache */
/* H5Pget_chunk_cache */

////////////////////////////////////////////////////
// Dataset Transfer Properties
////////////////////////////////////////////////////

/* H5Pset_buffer  */
/* H5Pget_buffer  */
/* H5Pset_preserve  *  */
/* H5Pget_preserve  *  */
/* H5Pset_edc_check  */
/* H5Pget_edc_check  */
/* H5Pset_filter_callback  */
/* H5Pset_data_transform  */
/* H5Pget_data_transform */
/* H5Pset_type_conv_cb  */
/* H5Pget_type_conv_cb  */
/* H5Pset_hyper_vector_size  */
/* H5Pget_hyper_vector_size  */
/* H5Pset_btree_ratios  */
/* H5Pget_btree_ratios  */
/* H5Pset_vlen_mem_manager  */
/* H5Pget_vlen_mem_manager */
/* mpi: H5Pset_dxpl_mpio   ||  */
/* mpi: H5Pset_dxpl_mpio_chunk_opt   ||  */
/* mpi: H5Pset_dxpl_mpio_chunk_opt_num   ||  */
/* mpi: H5Pset_dxpl_mpio_chunk_opt_ratio   ||  */
/* mpi: H5Pset_dxpl_mpio_collective_opt   ||  */
/* mpi: H5Pget_dxpl_mpio   ||  */
/* mpi: H5Pget_mpio_actual_chunk_opt_mode   ||  */
/* mpi: H5Pget_mpio_actual_io_mode   ||  */
/* mpi: H5Pget_mpio_no_collective_cause   || */

////////////////////////////////////////////////////
// Object Creation Properties
////////////////////////////////////////////////////

/* H5Pset_create_intermediate_group */
/* H5Pget_create_intermediate_group      */
/* H5Pset_obj_track_times */
/* H5Pget_obj_track_times  */
/* H5Pset_attr_phase_change */
/* H5Pget_attr_phase_change */
/* H5Pset_attr_creation_order */
/* H5Pget_attr_creation_order */

////////////////////////////////////////////////////
// Object Copy Properties
////////////////////////////////////////////////////

/* H5Pset_copy_object  */
/* H5Pget_copy_object */
/* H5Padd_merge_committed_dtype_path  */
/* H5Pfree_merge_committed_dtype_paths */
/* H5Pset_mcdt_search_cb  */
/* H5Pget_mcdt_search_cb */

////////////////////////////////////////////////////
// Attribute Creation Properties
////////////////////////////////////////////////////

/* H5Pset_char_encoding */
/* H5Pget_char_encoding */

////////////////////////////////////////////////////
// Generic Property Operations (Advanced)
////////////////////////////////////////////////////

/* H5Pcreate_class  */
/* H5Pregister  */
/* H5Pregister1  *  */
/* H5Pregister2  */
/* H5Pinsert  */
/* H5Pinsert1  *  */
/* H5Pinsert2 */
/* H5Pset  */
/* H5Pexist  */
/* H5Pget_size  */
/* H5Pget_nprops  */
/* H5Pget_class_name  */
/* H5Pget_class_parent  */
/* H5Pisa_class	H5Pget  */
/* H5Pequal  */
/* H5Piterate  */
/* H5Pcopy_prop  */
/* H5Premove  */
/* H5Punregister  */

/* herr_t H5Pclose_class( hid_t class ) */
SEXP _H5Pclose_class( SEXP _class ) {
  hid_t class =  INTEGER(_class)[0];
  herr_t herr = H5Pclose_class( class );
  if (herr == 0) {
    removeHandle(class);
  }

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = herr;
  UNPROTECT(1);
  return Rval;
}

