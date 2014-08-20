#ifndef _H5P_H
#define _H5P_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"
#include "HandleList.h"

/* General Property List Operations */

SEXP _H5Pcreate( SEXP _cls_id );
SEXP _H5Pget_class( SEXP _plist );
SEXP _H5Pcopy( SEXP _plist );
SEXP _H5Pclose( SEXP _plist );

/* File Access Properties */

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

/* Group Creation Properties */

/* H5Pset_local_heap_size_hint  */
/* H5Pget_local_heap_size_hint  */
/* H5Pset_link_creation_order */
/* H5Pget_link_creation_order      */
/* H5Pset_est_link_info  */
/* H5Pget_est_link_info */
/* H5Pset_link_phase_change  */
/* H5Pget_link_phase_change */

/* Link Creation Properties */

/* H5Pset_char_encoding */
/* H5Pget_char_encoding */
/* H5Pset_create_intermediate_group */
/* H5Pget_create_intermediate_group */

/* Link Access Properties */

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

/* Dataset Creation Properties */

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

/* Dataset Access Properties */

/* H5Pset_chunk_cache */
/* H5Pget_chunk_cache */

/* Dataset Transfer Properties */

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

/* Object Creation Properties */

/* H5Pset_create_intermediate_group */
/* H5Pget_create_intermediate_group      */
/* H5Pset_obj_track_times */
/* H5Pget_obj_track_times  */
/* H5Pset_attr_phase_change */
/* H5Pget_attr_phase_change */
/* H5Pset_attr_creation_order */
/* H5Pget_attr_creation_order */

/* Object Copy Properties */

/* H5Pset_copy_object  */
/* H5Pget_copy_object */
/* H5Padd_merge_committed_dtype_path  */
/* H5Pfree_merge_committed_dtype_paths */
/* H5Pset_mcdt_search_cb  */
/* H5Pget_mcdt_search_cb */

/* Attribute Creation Properties */

/* H5Pset_char_encoding */
/* H5Pget_char_encoding */

/* Generic Property Operations (Advanced) */

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
SEXP _H5Pclose_class( SEXP _class );


/* SEXP _H5Screate( SEXP _type ); */
/* SEXP _H5Scopy( SEXP _space_id ); */
/* SEXP _H5Sclose( SEXP _space_id ); */
/* /\\* H5Sdecode *\/ */
/* /\* H5Sencode *\/ */
/* SEXP _H5Screate_simple( SEXP _dims, SEXP _maxdims ); */
/* SEXP _H5Sis_simple( SEXP _space_id ); */
/* /\* H5Soffset_simple *\/ */
/* SEXP _H5Sget_simple_extent_dims( SEXP _space_id ); */
/* /\* H5Sget_simple_extent_ndims *\/ */
      	
/* /\* H5Sget_simple_extent_npoints *\/ */
/* /\* H5Sget_simple_extent_type *\/ */
/* /\* H5Sextent_copy *\/ */
/* /\* H5Sextent_equal *\/ */
/* /\* H5Sset_extent_simple *\/ */
/* /\* H5Sset_extent_none *\/ */
/* /\* H5Sget_select_type *\/ */
/* /\* H5Sget_select_npoints *\/ */
/* /\* H5Sget_select_hyper_nblocks *\/ */
/* /\* H5Sget_select_hyper_blocklist *\/ */
      	
/* /\* H5Sget_select_elem_npoints *\/ */
/* /\* H5Sget_select_elem_pointlist *\/ */
/* /\* H5Sget_select_bounds *\/ */
/* /\* H5Sselect_elements *\/ */
/* /\* H5Sselect_all *\/ */
/* /\* H5Sselect_none *\/ */
/* /\* H5Sselect_valid *\/ */
/* SEXP _H5Sselect_hyperslab( SEXP _space_id, SEXP _op, SEXP _start, SEXP _stride, SEXP _count, SEXP _block ); */
/* SEXP _H5Sselect_index( SEXP _space_id, SEXP _start, SEXP _count); */


#endif
