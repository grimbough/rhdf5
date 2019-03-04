#ifndef _H5P_H
#define _H5P_H

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>
#include "myhdf5.h"
#include "H5constants.h"
#include "HandleList.h"

////////////////////////////////////////////////////
// General Property List Operations
////////////////////////////////////////////////////

SEXP _H5Pcreate( SEXP _cls_id );
SEXP _H5Pget_class( SEXP _plist );
SEXP _H5Pcopy( SEXP _plist );
SEXP _H5Pclose( SEXP _plist );

////////////////////////////////////////////////////
// File Creation Properties
////////////////////////////////////////////////////

/* SEXP _H5Pget_version( SEXP _plist, SEXP _super, SEXP _freelist, SEXP _stab, SEXP _shhdr ); */
/* SEXP _H5Pset_userblock( SEXP _plist, SEXP _size ); */
/* SEXP _H5Pget_userblock( SEXP _plist, SEXP _size ); */
/* SEXP _H5Pset_sizes( SEXP _plist, SEXP _sizeof_addr, SEXP _sizeof_size ); */
/* SEXP _H5Pget_sizes( SEXP _plist, SEXP _sizeof_addr, SEXP _sizeof_size ); */
/* SEXP _H5Pset_sym_k( SEXP _fcpl_id, SEXP _ik, SEXP _lk ); */
/* SEXP _H5Pget_sym_k( SEXP _fcpl_id, SEXP _ik, SEXP _lk ); */
/* SEXP _H5Pset_istore_k( SEXP _fcpl_id, SEXP _ik ); */
/* SEXP _H5Pget_istore_k( SEXP _fcpl_id, SEXP _ik ); */
/* SEXP _H5Pset_shared_mesg_nindexes( SEXP _plist_id, SEXP _nindexes ); */
/* SEXP _H5Pget_shared_mesg_nindexes( SEXP _fcpl_id, SEXP _nindexes ); */
/* SEXP _H5Pset_shared_mesg_index( SEXP _fcpl_id, SEXP _index_num, SEXP _mesg_type_flags, SEXP _min_mesg_size ); */
/* SEXP _H5Pget_shared_mesg_index( SEXP _fcpl_id, SEXP _index_num, SEXP _mesg_type_flags, SEXP _min_mesg_size ); */
/* SEXP _H5Pset_shared_mesg_phase_change( SEXP _fcpl_id, SEXP _max_list, SEXP _min_btree ); */
/* SEXP _H5Pget_shared_mesg_phase_change( SEXP _fcpl_id, SEXP _max_list, SEXP _min_btree ); */

////////////////////////////////////////////////////
// File Access Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_driver( SEXP _plist_id, SEXP _new_driver_id, SEXP _new_driver_info ); */
/* SEXP _H5Pget_driver( SEXP _plist_id ); */
/* SEXP _H5Pget_driver_info( SEXP _plist_id ); */
/* SEXP _H5Pset_fclose_degree( SEXP _fapl_id, SEXP _fc_degree ); */
/* SEXP _H5Pget_fclose_degree( SEXP _fapl_id, SEXP _fc_degree ); */
/* SEXP _H5Pset_fapl_core( SEXP _fapl_id, SEXP _increment, SEXP _backing_store ); */
/* SEXP _H5Pget_fapl_core( SEXP _fapl_id, SEXP _increment, SEXP _backing_store ); */
/* SEXP _H5Pset_core_write_tracking( SEXP _fapl_id, SEXP _is_enabled, SEXP _page_size ); */
/* SEXP _H5Pget_core_write_tracking( SEXP _fapl_id, SEXP _is_enabled, SEXP _page_size ); */
/* SEXP _H5Pset_fapl_direct( SEXP _fapl_id, SEXP _alignment, SEXP _block_size, SEXP _cbuf_size ); */
/* SEXP _H5Pget_fapl_direct( SEXP _fapl_id, SEXP _alignment, SEXP _block_size, SEXP _cbuf_size ); */
/* SEXP _H5Pset_fapl_family( SEXP _fapl_id, SEXP _memb_size, SEXP _memb_fapl_id ); */
/* SEXP _H5Pget_fapl_family( SEXP _fapl_id, SEXP _memb_size, SEXP _memb_fapl_id ); */
/* SEXP _H5Pset_family_offset( SEXP _fapl_id, SEXP _offset ); */
/* SEXP _H5Pget_family_offset( SEXP _fapl_id, SEXP _offset ); */
/* SEXP _H5Pset_fapl_log( SEXP _fapl_id, SEXP _logfile, SEXP _flags, SEXP _buf_size ); */
/* SEXP _H5Pset_fapl_mpio( SEXP _fapl_id, SEXP _comm, SEXP _info ); */
/* SEXP _H5Pget_fapl_mpio( SEXP _fapl_id, SEXP _comm, SEXP _info ); */
/* SEXP _H5Pset_fapl_mpiposix( SEXP _fapl_id, SEXP _comm, SEXP _use_gpfs_hints ); */
/* SEXP _H5Pget_fapl_mpiposix( SEXP _fapl_id, SEXP _comm, SEXP _use_gpfs_hints ); */
/* SEXP _H5Pset_fapl_multi( SEXP _fapl_id, SEXP _memb_map, SEXP _memb_fapl, SEXP _memb_name, SEXP _memb_addr, SEXP _relax ); */
/* SEXP _H5Pget_fapl_multi( SEXP _fapl_id, SEXP _memb_map, SEXP _memb_fapl, SEXP _memb_name, SEXP _memb_addr, SEXP _relax ); */
/* SEXP _H5Pset_multi_type( SEXP _fapl_id, SEXP _type ); */
/* SEXP _H5Pget_multi_type( SEXP _fapl_id, SEXP _type ); */
/* SEXP _H5Pset_fapl_split( SEXP _fapl_id, SEXP _meta_ext, SEXP _meta_plist_id, SEXP _raw_ext, SEXP _raw_plist_id ); */
/* SEXP _H5Pset_fapl_sec2( SEXP _fapl_id ); */
/* SEXP _H5Pset_fapl_stdio( SEXP _fapl_id ); */
/* SEXP _H5Pset_fapl_windows( SEXP _fapl_id ); */
/* SEXP _H5Pset_file_image( SEXP _fapl_id, SEXP _buf_ptr, SEXP _buf_len ); */
/* SEXP _H5Pget_file_image( SEXP _fapl_id, SEXP _buf_ptr_ptr, SEXP _buf_len_ptr ); */
/* SEXP _H5Pset_file_image_callbacks( SEXP _fapl_id, SEXP _callbacks_ptr ); */
/* SEXP _H5Pget_file_image_callbacks( SEXP _fapl_id, SEXP _callbacks_ptr ); */
/* SEXP _H5Pset_meta_block_size( SEXP _fapl_id, SEXP _size ); */
/* SEXP _H5Pget_meta_block_size( SEXP _fapl_id, SEXP _size ); */
/* SEXP _H5Pset_sieve_buf_size( SEXP _fapl_id, SEXP _size ); */
/* SEXP _H5Pget_sieve_buf_size( SEXP _fapl_id, SEXP _size ); */
/* SEXP _H5Pset_alignment( SEXP _plist, SEXP _threshold, SEXP _alignment ); */
/* SEXP _H5Pget_alignment( SEXP _plist, SEXP _*threshold, SEXP _*alignment ); */
/* SEXP _H5Pset_cache( SEXP _plist_id, SEXP _mdc_nelmts, SEXP _rdcc_nslots, SEXP _rdcc_nbytes, SEXP _rdcc_w0 ); */
/* SEXP _H5Pget_cache( SEXP _plist_id, SEXP _*mdc_nelmts, SEXP _*rdcc_nelmts, SEXP _*rdcc_nbytes, SEXP _*rdcc_w0 ); */
/* SEXP _H5Pset_elink_file_cache_size( SEXP _fapl_id, SEXP _efc_size ); */
/* SEXP _H5Pget_elink_file_cache_size( SEXP _fapl_id, SEXP _efc_size ); */
/* SEXP _H5Pset_mdc_config( SEXP _plist_id, SEXP _config_ptr ); */
/* SEXP _H5Pget_mdc_config( SEXP _plist_id, SEXP _config_ptr ); */
/* SEXP _H5Pset_gc_reference( SEXP _plist, SEXP _gc_ref ); */
/* SEXP _H5Pget_gc_references( SEXP _plist, SEXP _gc_ref ); */
/* SEXP _H5Pset_small_data_block_size( SEXP _fapl_id, SEXP _size ); */
/* SEXP _H5Pget_small_data_block_size( SEXP _fapl_id, SEXP _size ); */
SEXP _H5Pset_libver_bounds( SEXP _fapl_id, SEXP _libver_low, SEXP _libver_high );
SEXP _H5Pget_libver_bounds( SEXP _fapl_id );

////////////////////////////////////////////////////
// Group Creation Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_local_heap_size_hint( SEXP _gcpl_id, SEXP _size_hint ); */
/* SEXP _H5Pget_local_heap_size_hint( SEXP _gcpl_id, SEXP _size_hint ); */
/* SEXP _H5Pset_link_creation_order( SEXP _gcpl_id, SEXP _crt_order_flags ); */
/* SEXP _H5Pget_link_creation_order( SEXP _gcpl_id, SEXP _crt_order_flags ); */
/* SEXP _H5Pset_est_link_info( SEXP _gcpl_id, SEXP _est_num_entries, SEXP _est_name_len ); */
/* SEXP _H5Pget_est_link_info( SEXP _gcpl_id, SEXP _est_num_entries, SEXP _est_name_len ); */
/* SEXP _H5Pset_link_phase_change( SEXP _gcpl_id, SEXP _max_compact, SEXP _min_dense ); */
/* SEXP _H5Pget_link_phase_change( SEXP _gcpl_id, SEXP _max_compact, SEXP _min_dense ); */

////////////////////////////////////////////////////
// Link Creation Properties
////////////////////////////////////////////////////

SEXP _H5Pset_char_encoding( SEXP _plist_id, SEXP _encoding );
SEXP _H5Pget_char_encoding( SEXP _plist_id );
SEXP _H5Pset_create_intermediate_group( SEXP _lcpl_id, SEXP _crt_intermed_group );
SEXP _H5Pget_create_intermediate_group( SEXP _lcpl_id );

////////////////////////////////////////////////////
// Link Access Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_nlinks( SEXP _lapl_id, SEXP _nlinks ); */
/* SEXP _H5Pget_nlinks( SEXP _lapl_id, SEXP _nlinks ); */
/* SEXP _H5Pset_elink_cb( SEXP _lapl_id, SEXP _func, SEXP _op_data ); */
/* SEXP _H5Pget_elink_cb( SEXP _lapl_id, SEXP _func, SEXP _op_data ); */
/* SEXP _H5Pset_elink_prefix( SEXP _lapl_id, SEXP _prefix ); */
/* SEXP _H5Pget_elink_prefix( SEXP _lapl_id, SEXP _prefix, SEXP _size ); */
/* SEXP _H5Pset_elink_fapl( SEXP _lapl_id, SEXP _fapl_id ); */
/* SEXP _H5Pget_elink_fapl( SEXP _lapl_id ); */
/* SEXP _H5Pset_elink_acc_flags( SEXP _lapl_id, SEXP _flags ); */
/* SEXP _H5Pget_elink_acc_flags( SEXP _lapl_id, SEXP _flags ); */

////////////////////////////////////////////////////
// Dataset Creation Properties
////////////////////////////////////////////////////

SEXP _H5Pset_layout( SEXP _plist, SEXP _layout );
SEXP _H5Pget_layout( SEXP _plist );
SEXP _H5Pset_chunk( SEXP _plist, SEXP _dim );
SEXP _H5Pget_chunk( SEXP _plist );
SEXP _H5Pset_deflate( SEXP _plist_id, SEXP _level );
SEXP _H5Pset_fill_value( SEXP _plist_id, SEXP _type_id, SEXP _value );
/* SEXP _H5Pget_fill_value( SEXP _plist_id, SEXP _type_id, SEXP _value ); */
SEXP _H5Pfill_value_defined( SEXP _plist_id );
SEXP _H5Pset_fill_time( SEXP _plist_id, SEXP _fill_time );
SEXP _H5Pget_fill_time( SEXP _plist_id );
SEXP _H5Pset_alloc_time( SEXP _plist_id, SEXP _alloc_time );
SEXP _H5Pget_alloc_time( SEXP _plist_id );
/* SEXP _H5Pset_filter( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[] ); */
SEXP _H5Pall_filters_avail( SEXP _plist_id );
SEXP _H5Pget_nfilters( SEXP _plist );
/* SEXP _H5Pget_filter1( SEXP _plist_id, SEXP _idx, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values, SEXP _namelen, SEXP _name[] ); */
/* SEXP _H5Pget_filter2( SEXP _plist_id, SEXP _idx, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[], SEXP _namelen, SEXP _name[], SEXP _filter_config ); */
/* SEXP _H5Pget_filter_by_id1( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[], SEXP _namelen, SEXP _name[] ); */
/* SEXP _H5Pget_filter_by_id2( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[], SEXP _namelen, SEXP _name[], SEXP _filter_config ); */
/* SEXP _H5Pmodify_filter( SEXP _plist_id, SEXP _filter_id, SEXP _flags, SEXP _cd_nelmts, SEXP _cd_values[] ); */
/* SEXP _H5Premove_filter( SEXP _plist_id, SEXP _filter ); */
/* SEXP _H5Pset_fletcher32( SEXP _plist_id ); */
/* SEXP _H5Pset_nbit( SEXP _plist_id ); */
/* SEXP _H5Pset_scaleoffset( SEXP _plist_id, SEXP _scale_type, SEXP _scale_factor ); */
/* SEXP _H5Pset_shuffle( SEXP _plist_id ); */
/* SEXP _H5Pset_szip( SEXP _plist, SEXP _options_mask, SEXP _pixels_per_block ); */
/* SEXP _H5Pset_external( SEXP _plist, SEXP _*name, SEXP _offset, SEXP _size ); */
/* SEXP _H5Pget_external_count( SEXP _plist ); */
/* SEXP _H5Pget_external( SEXP _plist, SEXP _idx, SEXP _name_size, SEXP _*name, SEXP _*offset, SEXP _*size ); */

////////////////////////////////////////////////////
// Dataset Access Properties
////////////////////////////////////////////////////

SEXP _H5Pset_chunk_cache( SEXP _dapl_id, SEXP _rdcc_nslots, SEXP _rdcc_nbytes, SEXP _rdcc_w0 );
/* SEXP _H5Pget_chunk_cache( SEXP _dapl_id );  // , SEXP _rdcc_nslots, SEXP _rdcc_nbytes, SEXP _rdcc_w0  */

////////////////////////////////////////////////////
// Dataset Transfer Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_buffer( SEXP _plist, SEXP _size, SEXP _*tconv, SEXP _*bkg ); */
/* SEXP _H5Pget_buffer( SEXP _plist, SEXP _**tconv, SEXP _**bkg ); */
/* SEXP _H5Pset_preserve( SEXP _plist, SEXP _status ); */
/* SEXP _H5Pget_preserve( SEXP _plist ); */
/* SEXP _H5Pset_edc_check( SEXP _plist, SEXP _check ); */
/* SEXP _H5Pget_edc_check( SEXP _plist ); */
/* SEXP _H5Pset_filter_callback( SEXP _plist, SEXP _func, SEXP _op_data ); */
/* SEXP _H5Pset_data_transform( SEXP _plist_id, SEXP _expression ); */
/* SEXP _H5Pget_data_transform( SEXP _plist_id, SEXP _expression, SEXP _size ); */
/* SEXP _H5Pset_type_conv_cb( SEXP _plist, SEXP _func, SEXP _op_data ); */
/* SEXP _H5Pget_type_conv_cb( SEXP _plist, SEXP _func, SEXP _op_data ); */
/* SEXP _H5Pset_hyper_vector_size( SEXP _dxpl_id, SEXP _vector_size ); */
/* SEXP _H5Pget_hyper_vector_size( SEXP _dxpl_id, SEXP _vector_size ); */
/* SEXP _H5Pset_btree_ratios( SEXP _plist, SEXP _left, SEXP _middle, SEXP _right ); */
/* SEXP _H5Pget_btree_ratios( SEXP _plist, SEXP _left, SEXP _middle, SEXP _right ); */
/* SEXP _H5Pset_vlen_mem_manager( SEXP _plist, SEXP _alloc, SEXP _alloc_info, SEXP _free, SEXP _free_info ); */
/* SEXP _H5Pget_vlen_mem_manager( SEXP _plist, SEXP _alloc, SEXP _alloc_info, SEXP _free, SEXP _free_info ); */
/* SEXP _H5Pset_dxpl_mpio( SEXP _dxpl_id, SEXP _xfer_mode ); */
/* SEXP _H5Pset_dxpl_mpio_chunk_opt( SEXP _dxpl_id, SEXP _opt_mode ); */
/* SEXP _H5Pset_dxpl_mpio_chunk_opt_num( SEXP _dxpl_id, SEXP _num_chunk_per_proc ); */
/* SEXP _H5Pset_dxpl_mpio_chunk_opt_ratio( SEXP _dxpl_id, SEXP _percent_proc_per_chunk ); */
/* SEXP _H5Pset_dxpl_mpio_collective_opt( SEXP _dxpl_id, SEXP _opt_mode ); */
/* SEXP _H5Pget_dxpl_mpio( SEXP _dxpl_id, SEXP _xfer_mode ); */
/* SEXP _H5Pget_mpio_actual_chunk_opt_mode( SEXP _dxpl_id, SEXP _actual_chunk_opt_mode ); */
/* SEXP _H5Pget_mpio_actual_io_mode( SEXP _dxpl_id, SEXP _actual_io_mode ); */
/* SEXP _H5Pget_mpio_no_collective_cause( SEXP _dxpl_id, SEXP _local_no_collective_cause, SEXP _global_no_collective_cause ); */

////////////////////////////////////////////////////
// Object Creation Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_create_intermediate_group( SEXP _lcpl_id, SEXP _crt_intermed_group ); */
/* SEXP _H5Pget_create_intermediate_group( SEXP _lcpl_id, SEXP _crt_intermed_group ); */
/* SEXP _H5Pset_obj_track_times( SEXP _ocpl_id, SEXP _track_times ); */
/* SEXP _H5Pget_obj_track_times( SEXP _ocpl_id, SEXP _track_times ); */
/* SEXP _H5Pset_attr_phase_change( SEXP _ocpl_id, SEXP _max_compact, SEXP _min_dense ); */
/* SEXP _H5Pget_attr_phase_change( SEXP _ocpl_id, SEXP _max_compact, SEXP _min_dense ); */
/* SEXP _H5Pset_attr_creation_order( SEXP _ocpl_id, SEXP _crt_order_flags ); */
/* SEXP _H5Pget_attr_creation_order( SEXP _ocpl_id, SEXP _crt_order_flags ); */

////////////////////////////////////////////////////
// Object Copy Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_copy_object( SEXP _ocpypl_id, SEXP _copy_options ); */
/* SEXP _H5Pget_copy_object( SEXP _ocp_plist_id, SEXP _copy_options ); */
/* SEXP _H5Padd_merge_committed_dtype_path( SEXP _ocpypl_id, SEXP _path ); */
/* SEXP _H5Pfree_merge_committed_dtype_paths( SEXP _ocpypl_id ); */
/* SEXP _H5Pset_mcdt_search_cb( SEXP _ocpypl_id, SEXP _func, SEXP _op_data ); */
/* SEXP _H5Pget_mcdt_search_cb( SEXP _ocpypl_id, SEXP _func, SEXP _op_data ); */

////////////////////////////////////////////////////
// Attribute Creation Properties
////////////////////////////////////////////////////

/* SEXP _H5Pset_char_encoding( SEXP _plist_id, SEXP _encoding ); */
/* SEXP _H5Pget_char_encoding( SEXP _plist_id, SEXP _encoding ); */

////////////////////////////////////////////////////
// Generic Property Operations (Advanced)
////////////////////////////////////////////////////

/* SEXP _H5Pcreate_class( SEXP _parent_class, SEXP _name, SEXP _create, SEXP _create_data, SEXP _copy, SEXP _copy_data, SEXP _close, SEXP _close_data ); */
/* SEXP _H5Pregister1( SEXP _class, SEXP _name, SEXP _size, SEXP _default, SEXP _create, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _close ); */
/* SEXP _H5Pregister2( SEXP _class, SEXP _name, SEXP _size, SEXP _default, SEXP _create, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _compare, SEXP _close ); */
/* SEXP _H5Pinsert1( SEXP _plid, SEXP _name, SEXP _size, SEXP _value, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _close ); */
/* SEXP _H5Pinsert2( SEXP _plid, SEXP _name, SEXP _size, SEXP _value, SEXP _set, SEXP _get, SEXP _delete, SEXP _copy, SEXP _compare, SEXP _close ); */
/* SEXP _H5Pset( SEXP _plid, SEXP _name, SEXP _value ); */
/* SEXP _H5Pexist( SEXP _id, SEXP _name ); */
/* SEXP _H5Pget_size( SEXP _id, SEXP _name, SEXP _size ); */
/* SEXP _H5Pget_nprops( SEXP _plist_id, SEXP _nprops ); */
/* SEXP _H5Pget_class_name( SEXP _pcid ); */
/* SEXP _H5Pget_class_parent( SEXP _pcid ); */
/* SEXP _H5Pisa_class( SEXP _plist, SEXP _pclass ); */
/* SEXP _H5Pget( SEXP _plid, SEXP _name, SEXP _value ); */
SEXP _H5Pequal( SEXP _id1, SEXP _id2 );
/* SEXP _H5Piterate( SEXP _id, SEXP _idx, SEXP _iter_func, SEXP _iter_data ); */
/* SEXP _H5Pcopy_prop( SEXP _dst_id, SEXP _src_id, SEXP _name ); */
/* SEXP _H5Premove( SEXP _plid, SEXP _name ); */
/* SEXP _H5Punregister( SEXP _class, SEXP _name ); */
SEXP _H5Pclose_class( SEXP _class );

#endif
