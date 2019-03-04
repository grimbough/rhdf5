
####################################################
## General Property List Operations
####################################################

H5Pcreate <- function( type = h5default("H5P"), native = FALSE ) {
  type <- h5checkConstants( "H5P", type )
  pid <- .Call("_H5Pcreate", type, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plist = new("H5IdComponent", ID = pid, native = native)
  } else {
    message("HDF5: unable to create property list")
    h5plist = FALSE
  }
  invisible(h5plist)
}

H5Pget_class <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  pclid <- .Call("_H5Pget_class", h5plist@ID, PACKAGE='rhdf5')
  if (pclid > 0) {
    h5plistclass = new("H5IdComponent", ID = pclid, native = h5plist@native)
  } else {
    message("HDF5: unable to get property list class")
    h5plistclass = FALSE
  }
  invisible(h5plistclass)
}

H5Pcopy <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  pid <- .Call("_H5Pcopy", h5plist@ID, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plistnew = new("H5IdComponent", ID = pid, native = h5plist@native)
  } else {
    message("HDF5: unable to copy property list")
    h5plistnew = FALSE
  }
  invisible(h5plistnew)
}

H5Pclose <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  invisible(.Call("_H5Pclose", h5plist@ID, PACKAGE='rhdf5'))
}

####################################################
## File Creation Properties
####################################################

## H5Pget_version <- function( plist, super, freelist, stab, shhdr ) {
##   h5checktype(h5plist, "plist")
##   TODO: super = as.TYPE(super)
##   TODO: freelist = as.TYPE(freelist)
##   TODO: stab = as.TYPE(stab)
##   TODO: shhdr = as.TYPE(shhdr)
##   res <- .Call("_H5Pget_version", plist, super, freelist, stab, shhdr, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_userblock <- function( plist, size ) {
##   h5checktype(h5plist, "plist")
##   hsize_t size = as.integer(size)
##   res <- .Call("_H5Pset_userblock", plist, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_userblock <- function( plist, size ) {
##   h5checktype(h5plist, "plist")
##   TODO: size = as.TYPE(size)
##   res <- .Call("_H5Pget_userblock", plist, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_sizes <- function( plist, sizeof_addr, sizeof_size ) {
##   h5checktype(h5plist, "plist")
##   sizeof_addr = as.integer(sizeof_addr)
##   sizeof_size = as.integer(sizeof_size)
##   res <- .Call("_H5Pset_sizes", plist, sizeof_addr, sizeof_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_sizes <- function( plist, sizeof_addr, sizeof_size ) {
##   h5checktype(h5plist, "plist")
##   TODO: sizeof_addr = as.TYPE(sizeof_addr)
##   TODO: sizeof_size = as.TYPE(sizeof_size)
##   res <- .Call("_H5Pget_sizes", plist, sizeof_addr, sizeof_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_sym_k <- function( fcpl_id, ik, lk ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   ik = as.integer(ik)
##   lk = as.integer(lk)
##   res <- .Call("_H5Pset_sym_k", fcpl_id, ik, lk, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_sym_k <- function( fcpl_id, ik, lk ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   TODO: ik = as.TYPE(ik)
##   TODO: lk = as.TYPE(lk)
##   res <- .Call("_H5Pget_sym_k", fcpl_id, ik, lk, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_istore_k <- function( fcpl_id, ik ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   ik = as.integer(ik)
##   res <- .Call("_H5Pset_istore_k", fcpl_id, ik, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_istore_k <- function( fcpl_id, ik ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   TODO: ik = as.TYPE(ik)
##   res <- .Call("_H5Pget_istore_k", fcpl_id, ik, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_shared_mesg_nindexes <- function( plist_id, nindexes ) {
##   h5checktype(h5plist, "plist")
##   nindexes = as.integer(nindexes)
##   res <- .Call("_H5Pset_shared_mesg_nindexes", plist_id, nindexes, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_shared_mesg_nindexes <- function( fcpl_id, nindexes ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   nindexes = as.integer(nindexes)
##   res <- .Call("_H5Pget_shared_mesg_nindexes", fcpl_id, nindexes, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_shared_mesg_index <- function( fcpl_id, index_num, mesg_type_flags, min_mesg_size ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   index_num = as.integer(index_num)
##   mesg_type_flags = as.integer(mesg_type_flags)
##   min_mesg_size = as.integer(min_mesg_size)
##   res <- .Call("_H5Pset_shared_mesg_index", fcpl_id, index_num, mesg_type_flags, min_mesg_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_shared_mesg_index <- function( fcpl_id, index_num, mesg_type_flags, min_mesg_size ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   index_num = as.integer(index_num)
##   mesg_type_flags = as.integer(mesg_type_flags)
##   min_mesg_size = as.integer(min_mesg_size)
##   res <- .Call("_H5Pget_shared_mesg_index", fcpl_id, index_num, mesg_type_flags, min_mesg_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_shared_mesg_phase_change <- function( fcpl_id, max_list, min_btree ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   max_list = as.integer(max_list)
##   min_btree = as.integer(min_btree)
##   res <- .Call("_H5Pset_shared_mesg_phase_change", fcpl_id, max_list, min_btree, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_shared_mesg_phase_change <- function( fcpl_id, max_list, min_btree ) {
##   TODO: fcpl_id = as.TYPE(fcpl_id)
##   max_list = as.integer(max_list)
##   min_btree = as.integer(min_btree)
##   res <- .Call("_H5Pget_shared_mesg_phase_change", fcpl_id, max_list, min_btree, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## File Access Properties
####################################################

## H5Pset_driver <- function( plist_id, new_driver_id, new_driver_info ) {
##   h5checktype(h5plist, "plist")
##   TODO: new_driver_id = as.TYPE(new_driver_id)
##   TODO: new_driver_info = as.TYPE(new_driver_info)
##   res <- .Call("_H5Pset_driver", plist_id, new_driver_id, new_driver_info, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_driver <- function( plist_id ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pget_driver", plist_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_driver_info <- function( plist_id ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pget_driver_info", plist_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fclose_degree <- function( fapl_id, fc_degree ) {
##   # TODO: check type fapl
##   TODO: fc_degree = as.TYPE(fc_degree)
##   res <- .Call("_H5Pset_fclose_degree", fapl_id, fc_degree, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fclose_degree <- function( fapl_id, fc_degree ) {
##   # TODO: check type fapl
##   TODO: fc_degree = as.TYPE(fc_degree)
##   res <- .Call("_H5Pget_fclose_degree", fapl_id, fc_degree, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_core <- function( fapl_id, increment, backing_store ) {
##   # TODO: check type fapl
##   increment = as.integer(increment)
##   TODO: backing_store = as.TYPE(backing_store)
##   res <- .Call("_H5Pset_fapl_core", fapl_id, increment, backing_store, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fapl_core <- function( fapl_id, increment, backing_store ) {
##   # TODO: check type fapl
##   increment = as.integer(increment)
##   TODO: backing_store = as.TYPE(backing_store)
##   res <- .Call("_H5Pget_fapl_core", fapl_id, increment, backing_store, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_core_write_tracking <- function( fapl_id, is_enabled, page_size ) {
##   # TODO: check type fapl
##   TODO: is_enabled = as.TYPE(is_enabled)
##   page_size = as.integer(page_size)
##   res <- .Call("_H5Pset_core_write_tracking", fapl_id, is_enabled, page_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_core_write_tracking <- function( fapl_id, is_enabled, page_size ) {
##   # TODO: check type fapl
##   TODO: is_enabled = as.TYPE(is_enabled)
##   page_size = as.integer(page_size)
##   res <- .Call("_H5Pget_core_write_tracking", fapl_id, is_enabled, page_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_direct <- function( fapl_id, alignment, block_size, cbuf_size ) {
##   # TODO: check type fapl
##   alignment = as.integer(alignment)
##   block_size = as.integer(block_size)
##   cbuf_size = as.integer(cbuf_size)
##   res <- .Call("_H5Pset_fapl_direct", fapl_id, alignment, block_size, cbuf_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fapl_direct <- function( fapl_id, alignment, block_size, cbuf_size ) {
##   # TODO: check type fapl
##   alignment = as.integer(alignment)
##   block_size = as.integer(block_size)
##   cbuf_size = as.integer(cbuf_size)
##   res <- .Call("_H5Pget_fapl_direct", fapl_id, alignment, block_size, cbuf_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_family <- function( fapl_id, memb_size, memb_fapl_id ) {
##   # TODO: check type fapl
##   hsize_t memb_size = as.integer(memb_size)
##   TODO: memb_fapl_id = as.TYPE(memb_fapl_id)
##   res <- .Call("_H5Pset_fapl_family", fapl_id, memb_size, memb_fapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fapl_family <- function( fapl_id, memb_size, memb_fapl_id ) {
##   # TODO: check type fapl
##   TODO: memb_size = as.TYPE(memb_size)
##   TODO: memb_fapl_id = as.TYPE(memb_fapl_id)
##   res <- .Call("_H5Pget_fapl_family", fapl_id, memb_size, memb_fapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_family_offset <- function( fapl_id, offset ) {
##   # TODO: check type fapl
##   hsize_t offset = as.integer(offset)
##   res <- .Call("_H5Pset_family_offset", fapl_id, offset, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_family_offset <- function( fapl_id, offset ) {
##   # TODO: check type fapl
##   TODO: offset = as.TYPE(offset)
##   res <- .Call("_H5Pget_family_offset", fapl_id, offset, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_log <- function( fapl_id, logfile, flags, buf_size ) {
##   # TODO: check type fapl
##   if (length(logfile)!=1 || !is.character(logfile)) stop("'logfile' must be a character string of length 1")
##   flags = as.integer(flags)
##   buf_size = as.integer(buf_size)
##   res <- .Call("_H5Pset_fapl_log", fapl_id, logfile, flags, buf_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_mpio <- function( fapl_id, comm, info ) {
##   # TODO: check type fapl
##   TODO: comm = as.TYPE(comm)
##   TODO: info = as.TYPE(info)
##   res <- .Call("_H5Pset_fapl_mpio", fapl_id, comm, info, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fapl_mpio <- function( fapl_id, comm, info ) {
##   # TODO: check type fapl
##   TODO: comm = as.TYPE(comm)
##   TODO: info = as.TYPE(info)
##   res <- .Call("_H5Pget_fapl_mpio", fapl_id, comm, info, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_mpiposix <- function( fapl_id, comm, use_gpfs_hints ) {
##   # TODO: check type fapl
##   TODO: comm = as.TYPE(comm)
##   TODO: use_gpfs_hints = as.TYPE(use_gpfs_hints)
##   res <- .Call("_H5Pset_fapl_mpiposix", fapl_id, comm, use_gpfs_hints, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fapl_mpiposix <- function( fapl_id, comm, use_gpfs_hints ) {
##   # TODO: check type fapl
##   TODO: comm = as.TYPE(comm)
##   TODO: use_gpfs_hints = as.TYPE(use_gpfs_hints)
##   res <- .Call("_H5Pget_fapl_mpiposix", fapl_id, comm, use_gpfs_hints, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_multi <- function( fapl_id, memb_map, memb_fapl, memb_name, memb_addr, relax ) {
##   # TODO: check type fapl
##   TODO: memb_map = as.TYPE(memb_map)
##   TODO: memb_fapl = as.TYPE(memb_fapl)
##   TODO: memb_name = as.TYPE(memb_name)
##   TODO: memb_addr = as.TYPE(memb_addr)
##   TODO: relax = as.TYPE(relax)
##   res <- .Call("_H5Pset_fapl_multi", fapl_id, memb_map, memb_fapl, memb_name, memb_addr, relax, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_fapl_multi <- function( fapl_id, memb_map, memb_fapl, memb_name, memb_addr, relax ) {
##   # TODO: check type fapl
##   TODO: memb_map = as.TYPE(memb_map)
##   TODO: memb_fapl = as.TYPE(memb_fapl)
##   TODO: memb_name = as.TYPE(memb_name)
##   TODO: memb_addr = as.TYPE(memb_addr)
##   TODO: relax = as.TYPE(relax)
##   res <- .Call("_H5Pget_fapl_multi", fapl_id, memb_map, memb_fapl, memb_name, memb_addr, relax, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_multi_type <- function( fapl_id, type ) {
##   # TODO: check type fapl
##   TODO: type = as.TYPE(type)
##   res <- .Call("_H5Pset_multi_type", fapl_id, type, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_multi_type <- function( fapl_id, type ) {
##   # TODO: check type fapl
##   TODO: type = as.TYPE(type)
##   res <- .Call("_H5Pget_multi_type", fapl_id, type, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_split <- function( fapl_id, meta_ext, meta_plist_id, raw_ext, raw_plist_id ) {
##   # TODO: check type fapl
##   if (length(meta_ext)!=1 || !is.character(meta_ext)) stop("'meta_ext' must be a character string of length 1")
##   TODO: meta_plist_id = as.TYPE(meta_plist_id)
##   if (length(raw_ext)!=1 || !is.character(raw_ext)) stop("'raw_ext' must be a character string of length 1")
##   TODO: raw_plist_id = as.TYPE(raw_plist_id)
##   res <- .Call("_H5Pset_fapl_split", fapl_id, meta_ext, meta_plist_id, raw_ext, raw_plist_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_sec2 <- function( fapl_id ) {
##   # TODO: check type fapl
##   res <- .Call("_H5Pset_fapl_sec2", fapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_stdio <- function( fapl_id ) {
##   # TODO: check type fapl
##   res <- .Call("_H5Pset_fapl_stdio", fapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fapl_windows <- function( fapl_id ) {
##   # TODO: check type fapl
##   res <- .Call("_H5Pset_fapl_windows", fapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_file_image <- function( fapl_id, buf_ptr, buf_len ) {
##   # TODO: check type fapl
##   TODO: buf_ptr = as.TYPE(buf_ptr)
##   buf_len = as.integer(buf_len)
##   res <- .Call("_H5Pset_file_image", fapl_id, buf_ptr, buf_len, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_file_image <- function( fapl_id, buf_ptr_ptr, buf_len_ptr ) {
##   # TODO: check type fapl
##   TODO: buf_ptr_ptr = as.TYPE(buf_ptr_ptr)
##   TODO: buf_len_ptr = as.TYPE(buf_len_ptr)
##   res <- .Call("_H5Pget_file_image", fapl_id, buf_ptr_ptr, buf_len_ptr, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_file_image_callbacks <- function( fapl_id, callbacks_ptr ) {
##   # TODO: check type fapl
##   TODO: callbacks_ptr = as.TYPE(callbacks_ptr)
##   res <- .Call("_H5Pset_file_image_callbacks", fapl_id, callbacks_ptr, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_file_image_callbacks <- function( fapl_id, callbacks_ptr ) {
##   # TODO: check type fapl
##   TODO: callbacks_ptr = as.TYPE(callbacks_ptr)
##   res <- .Call("_H5Pget_file_image_callbacks", fapl_id, callbacks_ptr, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_meta_block_size <- function( fapl_id, size ) {
##   # TODO: check type fapl
##   hsize_t size = as.integer(size)
##   res <- .Call("_H5Pset_meta_block_size", fapl_id, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_meta_block_size <- function( fapl_id, size ) {
##   # TODO: check type fapl
##   TODO: size = as.TYPE(size)
##   res <- .Call("_H5Pget_meta_block_size", fapl_id, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_sieve_buf_size <- function( fapl_id, size ) {
##   # TODO: check type fapl
##   size = as.integer(size)
##   res <- .Call("_H5Pset_sieve_buf_size", fapl_id, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_sieve_buf_size <- function( fapl_id, size ) {
##   # TODO: check type fapl
##   TODO: size = as.TYPE(size)
##   res <- .Call("_H5Pget_sieve_buf_size", fapl_id, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_alignment <- function( plist, threshold, alignment ) {
##   h5checktype(h5plist, "plist")
##   hsize_t threshold = as.integer(threshold)
##   hsize_t alignment = as.integer(alignment)
##   res <- .Call("_H5Pset_alignment", plist, threshold, alignment, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_alignment <- function( plist, *threshold, *alignment ) {
##   h5checktype(h5plist, "plist")
##   hsize_t *threshold = as.integer(*threshold)
##   hsize_t *alignment = as.integer(*alignment)
##   res <- .Call("_H5Pget_alignment", plist, *threshold, *alignment, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_cache <- function( plist_id, mdc_nelmts, rdcc_nslots, rdcc_nbytes, rdcc_w0 ) {
##   h5checktype(h5plist, "plist")
##   mdc_nelmts = as.integer(mdc_nelmts)
##   rdcc_nslots = as.integer(rdcc_nslots)
##   rdcc_nbytes = as.integer(rdcc_nbytes)
##   rdcc_w0 = as.double(rdcc_w0)
##   res <- .Call("_H5Pset_cache", plist_id, mdc_nelmts, rdcc_nslots, rdcc_nbytes, rdcc_w0, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_cache <- function( plist_id, *mdc_nelmts, *rdcc_nelmts, *rdcc_nbytes, *rdcc_w0 ) {
##   h5checktype(h5plist, "plist")
##   *mdc_nelmts = as.integer(*mdc_nelmts)
##   *rdcc_nelmts = as.integer(*rdcc_nelmts)
##   *rdcc_nbytes = as.integer(*rdcc_nbytes)
##   *rdcc_w0 = as.double(*rdcc_w0)
##   res <- .Call("_H5Pget_cache", plist_id, *mdc_nelmts, *rdcc_nelmts, *rdcc_nbytes, *rdcc_w0, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_elink_file_cache_size <- function( fapl_id, efc_size ) {
##   # TODO: check type fapl
##   efc_size = as.integer(efc_size)
##   res <- .Call("_H5Pset_elink_file_cache_size", fapl_id, efc_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_elink_file_cache_size <- function( fapl_id, efc_size ) {
##   # TODO: check type fapl
##   TODO: efc_size = as.TYPE(efc_size)
##   res <- .Call("_H5Pget_elink_file_cache_size", fapl_id, efc_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_mdc_config <- function( plist_id, config_ptr ) {
##   h5checktype(h5plist, "plist")
##   TODO: config_ptr = as.TYPE(config_ptr)
##   res <- .Call("_H5Pset_mdc_config", plist_id, config_ptr, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_mdc_config <- function( plist_id, config_ptr ) {
##   h5checktype(h5plist, "plist")
##   TODO: config_ptr = as.TYPE(config_ptr)
##   res <- .Call("_H5Pget_mdc_config", plist_id, config_ptr, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_gc_reference <- function( plist, gc_ref ) {
##   h5checktype(h5plist, "plist")
##   gc_ref = as.integer(gc_ref)
##   res <- .Call("_H5Pset_gc_reference", plist, gc_ref, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_gc_references <- function( plist, gc_ref ) {
##   h5checktype(h5plist, "plist")
##   gc_ref = as.integer(gc_ref)
##   res <- .Call("_H5Pget_gc_references", plist, gc_ref, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_small_data_block_size <- function( fapl_id, size ) {
##   # TODO: check type fapl
##   hsize_t size = as.integer(size)
##   res <- .Call("_H5Pset_small_data_block_size", fapl_id, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_small_data_block_size <- function( fapl_id, size ) {
##   # TODO: check type fapl
##   TODO: size = as.TYPE(size)
##   res <- .Call("_H5Pget_small_data_block_size", fapl_id, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

H5Pset_libver_bounds <- function( h5plist, libver_low = "H5F_LIBVER_EARLIEST", libver_high = "H5F_LIBVER_LATEST") {
  h5checktype(h5plist, "plist")
  libver_low <- h5checkConstants( "H5F_LIBVER", libver_low )
  libver_high <- h5checkConstants( "H5F_LIBVER", libver_high )
  res <- .Call("_H5Pset_libver_bounds", h5plist@ID, 
               libver_low, libver_high, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_libver_bounds <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  res <- .Call("_H5Pget_libver_bounds", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5F_LIBVER", res)
  names(res) = c("libver_low","libver_high")
  res
}

####################################################
## Group Creation Properties
####################################################

## H5Pset_local_heap_size_hint <- function( gcpl_id, size_hint ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   size_hint = as.integer(size_hint)
##   res <- .Call("_H5Pset_local_heap_size_hint", gcpl_id, size_hint, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_local_heap_size_hint <- function( gcpl_id, size_hint ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   TODO: size_hint = as.TYPE(size_hint)
##   res <- .Call("_H5Pget_local_heap_size_hint", gcpl_id, size_hint, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_link_creation_order <- function( gcpl_id, crt_order_flags ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   crt_order_flags = as.integer(crt_order_flags)
##   res <- .Call("_H5Pset_link_creation_order", gcpl_id, crt_order_flags, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_link_creation_order <- function( gcpl_id, crt_order_flags ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   crt_order_flags = as.integer(crt_order_flags)
##   res <- .Call("_H5Pget_link_creation_order", gcpl_id, crt_order_flags, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_est_link_info <- function( gcpl_id, est_num_entries, est_name_len ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   est_num_entries = as.integer(est_num_entries)
##   est_name_len = as.integer(est_name_len)
##   res <- .Call("_H5Pset_est_link_info", gcpl_id, est_num_entries, est_name_len, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_est_link_info <- function( gcpl_id, est_num_entries, est_name_len ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   est_num_entries = as.integer(est_num_entries)
##   est_name_len = as.integer(est_name_len)
##   res <- .Call("_H5Pget_est_link_info", gcpl_id, est_num_entries, est_name_len, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_link_phase_change <- function( gcpl_id, max_compact, min_dense ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   max_compact = as.integer(max_compact)
##   min_dense = as.integer(min_dense)
##   res <- .Call("_H5Pset_link_phase_change", gcpl_id, max_compact, min_dense, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_link_phase_change <- function( gcpl_id, max_compact, min_dense ) {
##   TODO: gcpl_id = as.TYPE(gcpl_id)
##   max_compact = as.integer(max_compact)
##   min_dense = as.integer(min_dense)
##   res <- .Call("_H5Pget_link_phase_change", gcpl_id, max_compact, min_dense, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Link Creation Properties
####################################################

H5Pset_char_encoding <- function( h5plist, encoding = h5default("H5T_CSET")) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  encoding <- h5checkConstants( "H5T_CSET", encoding )
  res <- .Call("_H5Pset_char_encoding", h5plist@ID, encoding, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_char_encoding <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  res <- .Call("_H5Pget_char_encoding", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5T_CSET", res)
  res
}

H5Pset_create_intermediate_group <- function( h5plist, crt_intermed_group ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  crt_intermed_group = as.integer(crt_intermed_group)
  res <- .Call("_H5Pset_create_intermediate_group", h5plist@ID, crt_intermed_group, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_create_intermediate_group <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  res <- .Call("_H5Pget_create_intermediate_group", h5plist@ID, PACKAGE='rhdf5')
  res
}

####################################################
## Link Access Properties
####################################################

## H5Pset_nlinks <- function( lapl_id, nlinks ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   nlinks = as.integer(nlinks)
##   res <- .Call("_H5Pset_nlinks", lapl_id, nlinks, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_nlinks <- function( lapl_id, nlinks ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   TODO: nlinks = as.TYPE(nlinks)
##   res <- .Call("_H5Pget_nlinks", lapl_id, nlinks, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_elink_cb <- function( lapl_id, func, op_data ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pset_elink_cb", lapl_id, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_elink_cb <- function( lapl_id, func, op_data ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pget_elink_cb", lapl_id, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_elink_prefix <- function( lapl_id, prefix ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   if (length(prefix)!=1 || !is.character(prefix)) stop("'prefix' must be a character string of length 1")
##   res <- .Call("_H5Pset_elink_prefix", lapl_id, prefix, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_elink_prefix <- function( lapl_id, prefix, size ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   if (length(prefix)!=1 || !is.character(prefix)) stop("'prefix' must be a character string of length 1")
##   size = as.integer(size)
##   res <- .Call("_H5Pget_elink_prefix", lapl_id, prefix, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_elink_fapl <- function( lapl_id, fapl_id ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   # TODO: check type fapl
##   res <- .Call("_H5Pset_elink_fapl", lapl_id, fapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_elink_fapl <- function( lapl_id ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   res <- .Call("_H5Pget_elink_fapl", lapl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_elink_acc_flags <- function( lapl_id, flags ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   flags = as.integer(flags)
##   res <- .Call("_H5Pset_elink_acc_flags", lapl_id, flags, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_elink_acc_flags <- function( lapl_id, flags ) {
##   TODO: lapl_id = as.TYPE(lapl_id)
##   TODO: flags = as.TYPE(flags)
##   res <- .Call("_H5Pget_elink_acc_flags", lapl_id, flags, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Dataset Creation Properties
####################################################

H5Pset_layout <- function( h5plist, layout = h5default("H5D") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  layout <- h5checkConstants( "H5D", layout )
  res <- .Call("_H5Pset_layout", h5plist@ID, layout, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_layout <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_layout", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D", res)
  res
}

H5Pset_chunk <- function( h5plist, dim ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  if (!is.null(dim) && !h5plist@native) { dim = rev(as.integer(dim)) }
  res <- .Call("_H5Pset_chunk", h5plist@ID, dim, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_chunk <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_chunk", h5plist@ID, PACKAGE='rhdf5')
  res
}

H5Pset_deflate <- function( h5plist, level ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  level = as.integer(level)
  res <- .Call("_H5Pset_deflate", h5plist@ID, level, PACKAGE='rhdf5')
  invisible(res)
}

H5Pset_fill_value <- function( h5plist, value ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  # TODO: check if class is H5P_DATASET_CREATE
  ## a = H5Pget_class(p)
  ## H5Pget_class
  storage.mode = storage.mode(value)
  tid <- switch(storage.mode,
                double = h5constants$H5T["H5T_NATIVE_DOUBLE"],
                integer = h5constants$H5T["H5T_NATIVE_INT32"],
                logical = h5constants$H5T["H5T_NATIVE_INT32"],
                character = {
                    tid <- H5Tcopy("H5T_C_S1")
                    size <- nchar(value)+1
                    H5Tset_size(tid, size)
                    tid
                },
                { stop("datatype ",storage.mode," not supported. Try 'double', 'integer', or 'character'.") } )
  res <- .Call("_H5Pset_fill_value", h5plist@ID, tid, value, PACKAGE='rhdf5')
  invisible(res)
}

## H5Pget_fill_value <- function( h5plist, type_id, value ) {
## h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
##   TODO: type_id = as.TYPE(type_id)
##   TODO: value = as.TYPE(value)
##   res <- .Call("_H5Pget_fill_value", h5plist@ID, type_id, value, PACKAGE='rhdf5')
##   invisible(res)
## }

H5Pfill_value_defined <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pfill_value_defined", h5plist@ID, PACKAGE='rhdf5')
  res
}

H5Pset_fill_time <- function( h5plist, fill_time = h5default("H5D_FILL_TIME") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  fill_time <- h5checkConstants( "H5D_FILL_TIME", fill_time )
  res <- .Call("_H5Pset_fill_time", h5plist@ID, fill_time, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_fill_time <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_fill_time", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D_FILL_TIME", res)
  res
}

H5Pset_alloc_time <- function( h5plist, alloc_time = h5default("H5D_ALLOC_TIME") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  alloc_time <- h5checkConstants( "H5D_ALLOC_TIME", alloc_time )
  res <- .Call("_H5Pset_alloc_time", h5plist@ID, alloc_time, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_alloc_time <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_alloc_time", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D_ALLOC_TIME", res)
  res
}

## H5Pset_filter <- function( plist_id, filter_id, flags, cd_nelmts, cd_values[] ) {
##   h5checktype(h5plist, "plist")
##   TODO: filter_id = as.TYPE(filter_id)
##   flags = as.integer(flags)
##   cd_nelmts = as.integer(cd_nelmts)
##   cd_values[] = as.integer(cd_values[])
##   res <- .Call("_H5Pset_filter", plist_id, filter_id, flags, cd_nelmts, cd_values[], PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

H5Pall_filters_avail <- function( h5plist ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pall_filters_avail", h5plist@ID, PACKAGE='rhdf5')
    return(res)
}

H5Pget_nfilters <- function( h5plist ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pget_nfilters", h5plist@ID, PACKAGE='rhdf5')
    res
}

## H5Pget_filter1 <- function( plist_id, idx, flags, cd_nelmts, cd_values, namelen, name[] ) {
##   h5checktype(h5plist, "plist")
##   idx = as.integer(idx)
##   TODO: flags = as.TYPE(flags)
##   TODO: cd_nelmts = as.TYPE(cd_nelmts)
##   TODO: cd_values = as.TYPE(cd_values)
##   namelen = as.integer(namelen)
##   if (length(name[])!=1 || !is.character(name[])) stop("'name[]' must be a character string of length 1")
##   res <- .Call("_H5Pget_filter1", plist_id, idx, flags, cd_nelmts, cd_values, namelen, name[], PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_filter2 <- function( plist_id, idx, flags, cd_nelmts, cd_values[], namelen, name[], filter_config ) {
##   h5checktype(h5plist, "plist")
##   idx = as.integer(idx)
##   TODO: flags = as.TYPE(flags)
##   TODO: cd_nelmts = as.TYPE(cd_nelmts)
##   cd_values[] = as.integer(cd_values[])
##   namelen = as.integer(namelen)
##   if (length(name[])!=1 || !is.character(name[])) stop("'name[]' must be a character string of length 1")
##   TODO: filter_config = as.TYPE(filter_config)
##   res <- .Call("_H5Pget_filter2", plist_id, idx, flags, cd_nelmts, cd_values[], namelen, name[], filter_config, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_filter_by_id1 <- function( plist_id, filter_id, flags, cd_nelmts, cd_values[], namelen, name[] ) {
##   h5checktype(h5plist, "plist")
##   TODO: filter_id = as.TYPE(filter_id)
##   TODO: flags = as.TYPE(flags)
##   TODO: cd_nelmts = as.TYPE(cd_nelmts)
##   cd_values[] = as.integer(cd_values[])
##   namelen = as.integer(namelen)
##   if (length(name[])!=1 || !is.character(name[])) stop("'name[]' must be a character string of length 1")
##   res <- .Call("_H5Pget_filter_by_id1", plist_id, filter_id, flags, cd_nelmts, cd_values[], namelen, name[], PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_filter_by_id2 <- function( plist_id, filter_id, flags, cd_nelmts, cd_values[], namelen, name[], filter_config ) {
##   h5checktype(h5plist, "plist")
##   TODO: filter_id = as.TYPE(filter_id)
##   TODO: flags = as.TYPE(flags)
##   TODO: cd_nelmts = as.TYPE(cd_nelmts)
##   cd_values[] = as.integer(cd_values[])
##   namelen = as.integer(namelen)
##   if (length(name[])!=1 || !is.character(name[])) stop("'name[]' must be a character string of length 1")
##   TODO: filter_config = as.TYPE(filter_config)
##   res <- .Call("_H5Pget_filter_by_id2", plist_id, filter_id, flags, cd_nelmts, cd_values[], namelen, name[], filter_config, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pmodify_filter <- function( plist_id, filter_id, flags, cd_nelmts, cd_values[] ) {
##   h5checktype(h5plist, "plist")
##   TODO: filter_id = as.TYPE(filter_id)
##   flags = as.integer(flags)
##   cd_nelmts = as.integer(cd_nelmts)
##   cd_values[] = as.integer(cd_values[])
##   res <- .Call("_H5Pmodify_filter", plist_id, filter_id, flags, cd_nelmts, cd_values[], PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Premove_filter <- function( plist_id, filter ) {
##   h5checktype(h5plist, "plist")
##   TODO: filter = as.TYPE(filter)
##   res <- .Call("_H5Premove_filter", plist_id, filter, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_fletcher32 <- function( plist_id ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pset_fletcher32", plist_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_nbit <- function( plist_id ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pset_nbit", plist_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_scaleoffset <- function( plist_id, scale_type, scale_factor ) {
##   h5checktype(h5plist, "plist")
##   TODO: scale_type = as.TYPE(scale_type)
##   scale_factor = as.integer(scale_factor)
##   res <- .Call("_H5Pset_scaleoffset", plist_id, scale_type, scale_factor, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_shuffle <- function( plist_id ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pset_shuffle", plist_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_szip <- function( plist, options_mask, pixels_per_block ) {
##   h5checktype(h5plist, "plist")
##   options_mask = as.integer(options_mask)
##   pixels_per_block = as.integer(pixels_per_block)
##   res <- .Call("_H5Pset_szip", plist, options_mask, pixels_per_block, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_external <- function( plist, *name, offset, size ) {
##   h5checktype(h5plist, "plist")
##   *name = as.integer(*name)
##   TODO: offset = as.TYPE(offset)
##   hsize_t size = as.integer(size)
##   res <- .Call("_H5Pset_external", plist, *name, offset, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_external_count <- function( plist ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pget_external_count", plist, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_external <- function( plist, idx, name_size, *name, *offset, *size ) {
##   h5checktype(h5plist, "plist")
##   idx = as.integer(idx)
##   name_size = as.integer(name_size)
##   if (length(*name)!=1 || !is.character(*name)) stop("'*name' must be a character string of length 1")
##   TODO: *offset = as.TYPE(*offset)
##   hsize_t *size = as.integer(*size)
##   res <- .Call("_H5Pget_external", plist, idx, name_size, *name, *offset, *size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Dataset Access Properties
####################################################

H5Pset_chunk_cache <- function( h5plist, rdcc_nslots, rdcc_nbytes, rdcc_w0 ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_ACCESS")
  rdcc_nslots = as.integer(rdcc_nslots)
  rdcc_nbytes = as.integer(rdcc_nbytes)
  rdcc_w0 = as.double(rdcc_w0)
  res <- .Call("_H5Pset_chunk_cache", h5plist@ID, rdcc_nslots, rdcc_nbytes, rdcc_w0, PACKAGE='rhdf5')
  invisible(res)
}

## H5Pget_chunk_cache <- function( h5plist ) {  # OUT: , rdcc_nslots, rdcc_nbytes, rdcc_w0
##   h5checktypeAndPLC(h5plist, "H5P_DATASET_ACCESS")
##   res <- .Call("_H5Pget_chunk_cache", h5plist@ID, PACKAGE='rhdf5')
##   invisible(res)
## }

####################################################
## Dataset Transfer Properties
####################################################

## H5Pset_buffer <- function( plist, size, *tconv, *bkg ) {
##   h5checktype(h5plist, "plist")
##   hsize_t size = as.integer(size)
##   TODO: *tconv = as.TYPE(*tconv)
##   TODO: *bkg = as.TYPE(*bkg)
##   res <- .Call("_H5Pset_buffer", plist, size, *tconv, *bkg, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_buffer <- function( plist, **tconv, **bkg ) {
##   h5checktype(h5plist, "plist")
##   TODO: **tconv = as.TYPE(**tconv)
##   TODO: **bkg = as.TYPE(**bkg)
##   res <- .Call("_H5Pget_buffer", plist, **tconv, **bkg, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_preserve <- function( plist, status ) {
##   h5checktype(h5plist, "plist")
##   TODO: status = as.TYPE(status)
##   res <- .Call("_H5Pset_preserve", plist, status, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_preserve <- function( plist ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pget_preserve", plist, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_edc_check <- function( plist, check ) {
##   h5checktype(h5plist, "plist")
##   TODO: check = as.TYPE(check)
##   res <- .Call("_H5Pset_edc_check", plist, check, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_edc_check <- function( plist ) {
##   h5checktype(h5plist, "plist")
##   res <- .Call("_H5Pget_edc_check", plist, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_filter_callback <- function( plist, func, op_data ) {
##   h5checktype(h5plist, "plist")
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pset_filter_callback", plist, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_data_transform <- function( plist_id, expression ) {
##   h5checktype(h5plist, "plist")
##   expression = as.integer(expression)
##   res <- .Call("_H5Pset_data_transform", plist_id, expression, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_data_transform <- function( plist_id, expression, size ) {
##   h5checktype(h5plist, "plist")
##   if (length(expression)!=1 || !is.character(expression)) stop("'expression' must be a character string of length 1")
##   size = as.integer(size)
##   res <- .Call("_H5Pget_data_transform", plist_id, expression, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_type_conv_cb <- function( plist, func, op_data ) {
##   h5checktype(h5plist, "plist")
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pset_type_conv_cb", plist, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_type_conv_cb <- function( plist, func, op_data ) {
##   h5checktype(h5plist, "plist")
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pget_type_conv_cb", plist, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_hyper_vector_size <- function( dxpl_id, vector_size ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   vector_size = as.integer(vector_size)
##   res <- .Call("_H5Pset_hyper_vector_size", dxpl_id, vector_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_hyper_vector_size <- function( dxpl_id, vector_size ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: vector_size = as.TYPE(vector_size)
##   res <- .Call("_H5Pget_hyper_vector_size", dxpl_id, vector_size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_btree_ratios <- function( plist, left, middle, right ) {
##   h5checktype(h5plist, "plist")
##   left = as.double(left)
##   middle = as.double(middle)
##   right = as.double(right)
##   res <- .Call("_H5Pset_btree_ratios", plist, left, middle, right, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_btree_ratios <- function( plist, left, middle, right ) {
##   h5checktype(h5plist, "plist")
##   left = as.double(left)
##   middle = as.double(middle)
##   right = as.double(right)
##   res <- .Call("_H5Pget_btree_ratios", plist, left, middle, right, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_vlen_mem_manager <- function( plist, alloc, alloc_info, free, free_info ) {
##   h5checktype(h5plist, "plist")
##   TODO: alloc = as.TYPE(alloc)
##   TODO: alloc_info = as.TYPE(alloc_info)
##   TODO: free = as.TYPE(free)
##   TODO: free_info = as.TYPE(free_info)
##   res <- .Call("_H5Pset_vlen_mem_manager", plist, alloc, alloc_info, free, free_info, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_vlen_mem_manager <- function( plist, alloc, alloc_info, free, free_info ) {
##   h5checktype(h5plist, "plist")
##   TODO: alloc = as.TYPE(alloc)
##   TODO: alloc_info = as.TYPE(alloc_info)
##   TODO: free = as.TYPE(free)
##   TODO: free_info = as.TYPE(free_info)
##   res <- .Call("_H5Pget_vlen_mem_manager", plist, alloc, alloc_info, free, free_info, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_dxpl_mpio <- function( dxpl_id, xfer_mode ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: xfer_mode = as.TYPE(xfer_mode)
##   res <- .Call("_H5Pset_dxpl_mpio", dxpl_id, xfer_mode, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_dxpl_mpio_chunk_opt <- function( dxpl_id, opt_mode ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: opt_mode = as.TYPE(opt_mode)
##   res <- .Call("_H5Pset_dxpl_mpio_chunk_opt", dxpl_id, opt_mode, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_dxpl_mpio_chunk_opt_num <- function( dxpl_id, num_chunk_per_proc ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   num_chunk_per_proc = as.integer(num_chunk_per_proc)
##   res <- .Call("_H5Pset_dxpl_mpio_chunk_opt_num", dxpl_id, num_chunk_per_proc, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_dxpl_mpio_chunk_opt_ratio <- function( dxpl_id, percent_proc_per_chunk ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   percent_proc_per_chunk = as.integer(percent_proc_per_chunk)
##   res <- .Call("_H5Pset_dxpl_mpio_chunk_opt_ratio", dxpl_id, percent_proc_per_chunk, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_dxpl_mpio_collective_opt <- function( dxpl_id, opt_mode ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: opt_mode = as.TYPE(opt_mode)
##   res <- .Call("_H5Pset_dxpl_mpio_collective_opt", dxpl_id, opt_mode, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_dxpl_mpio <- function( dxpl_id, xfer_mode ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: xfer_mode = as.TYPE(xfer_mode)
##   res <- .Call("_H5Pget_dxpl_mpio", dxpl_id, xfer_mode, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_mpio_actual_chunk_opt_mode <- function( dxpl_id, actual_chunk_opt_mode ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: actual_chunk_opt_mode = as.TYPE(actual_chunk_opt_mode)
##   res <- .Call("_H5Pget_mpio_actual_chunk_opt_mode", dxpl_id, actual_chunk_opt_mode, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_mpio_actual_io_mode <- function( dxpl_id, actual_io_mode ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: actual_io_mode = as.TYPE(actual_io_mode)
##   res <- .Call("_H5Pget_mpio_actual_io_mode", dxpl_id, actual_io_mode, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_mpio_no_collective_cause <- function( dxpl_id, local_no_collective_cause, global_no_collective_cause ) {
##   TODO: dxpl_id = as.TYPE(dxpl_id)
##   TODO: local_no_collective_cause = as.TYPE(local_no_collective_cause)
##   TODO: global_no_collective_cause = as.TYPE(global_no_collective_cause)
##   res <- .Call("_H5Pget_mpio_no_collective_cause", dxpl_id, local_no_collective_cause, global_no_collective_cause, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Object Creation Properties
####################################################

## H5Pset_create_intermediate_group <- function( lcpl_id, crt_intermed_group ) {
##   TODO: lcpl_id = as.TYPE(lcpl_id)
##   crt_intermed_group = as.integer(crt_intermed_group)
##   res <- .Call("_H5Pset_create_intermediate_group", lcpl_id, crt_intermed_group, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_create_intermediate_group <- function( lcpl_id, crt_intermed_group ) {
##   TODO: lcpl_id = as.TYPE(lcpl_id)
##   TODO: crt_intermed_group = as.TYPE(crt_intermed_group)
##   res <- .Call("_H5Pget_create_intermediate_group", lcpl_id, crt_intermed_group, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_obj_track_times <- function( ocpl_id, track_times ) {
##   TODO: ocpl_id = as.TYPE(ocpl_id)
##   TODO: track_times = as.TYPE(track_times)
##   res <- .Call("_H5Pset_obj_track_times", ocpl_id, track_times, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_obj_track_times <- function( ocpl_id, track_times ) {
##   TODO: ocpl_id = as.TYPE(ocpl_id)
##   TODO: track_times = as.TYPE(track_times)
##   res <- .Call("_H5Pget_obj_track_times", ocpl_id, track_times, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_attr_phase_change <- function( ocpl_id, max_compact, min_dense ) {
##   TODO: ocpl_id = as.TYPE(ocpl_id)
##   max_compact = as.integer(max_compact)
##   min_dense = as.integer(min_dense)
##   res <- .Call("_H5Pset_attr_phase_change", ocpl_id, max_compact, min_dense, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_attr_phase_change <- function( ocpl_id, max_compact, min_dense ) {
##   TODO: ocpl_id = as.TYPE(ocpl_id)
##   max_compact = as.integer(max_compact)
##   min_dense = as.integer(min_dense)
##   res <- .Call("_H5Pget_attr_phase_change", ocpl_id, max_compact, min_dense, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_attr_creation_order <- function( ocpl_id, crt_order_flags ) {
##   TODO: ocpl_id = as.TYPE(ocpl_id)
##   crt_order_flags = as.integer(crt_order_flags)
##   res <- .Call("_H5Pset_attr_creation_order", ocpl_id, crt_order_flags, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_attr_creation_order <- function( ocpl_id, crt_order_flags ) {
##   TODO: ocpl_id = as.TYPE(ocpl_id)
##   crt_order_flags = as.integer(crt_order_flags)
##   res <- .Call("_H5Pget_attr_creation_order", ocpl_id, crt_order_flags, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Object Copy Properties
####################################################

## H5Pset_copy_object <- function( ocpypl_id, copy_options ) {
##   TODO: ocpypl_id = as.TYPE(ocpypl_id)
##   copy_options = as.integer(copy_options)
##   res <- .Call("_H5Pset_copy_object", ocpypl_id, copy_options, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_copy_object <- function( ocp_plist_id, copy_options ) {
##   TODO: ocp_plist_id = as.TYPE(ocp_plist_id)
##   TODO: copy_options = as.TYPE(copy_options)
##   res <- .Call("_H5Pget_copy_object", ocp_plist_id, copy_options, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Padd_merge_committed_dtype_path <- function( ocpypl_id, path ) {
##   TODO: ocpypl_id = as.TYPE(ocpypl_id)
##   if (length(path)!=1 || !is.character(path)) stop("'path' must be a character string of length 1")
##   res <- .Call("_H5Padd_merge_committed_dtype_path", ocpypl_id, path, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pfree_merge_committed_dtype_paths <- function( ocpypl_id ) {
##   TODO: ocpypl_id = as.TYPE(ocpypl_id)
##   res <- .Call("_H5Pfree_merge_committed_dtype_paths", ocpypl_id, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset_mcdt_search_cb <- function( ocpypl_id, func, op_data ) {
##   TODO: ocpypl_id = as.TYPE(ocpypl_id)
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pset_mcdt_search_cb", ocpypl_id, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_mcdt_search_cb <- function( ocpypl_id, func, op_data ) {
##   TODO: ocpypl_id = as.TYPE(ocpypl_id)
##   TODO: func = as.TYPE(func)
##   TODO: op_data = as.TYPE(op_data)
##   res <- .Call("_H5Pget_mcdt_search_cb", ocpypl_id, func, op_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Attribute Creation Properties
####################################################

## H5Pset_char_encoding <- function( plist_id, encoding ) {
##   h5checktype(h5plist, "plist")
##   TODO: encoding = as.TYPE(encoding)
##   res <- .Call("_H5Pset_char_encoding", plist_id, encoding, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_char_encoding <- function( plist_id, encoding ) {
##   h5checktype(h5plist, "plist")
##   TODO: encoding = as.TYPE(encoding)
##   res <- .Call("_H5Pget_char_encoding", plist_id, encoding, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

####################################################
## Generic Property Operations (Advanced)
####################################################

## H5Pcreate_class <- function( parent_class, name, create, create_data, copy, copy_data, close, close_data ) {
##   TODO: parent_class = as.TYPE(parent_class)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   TODO: create = as.TYPE(create)
##   TODO: create_data = as.TYPE(create_data)
##   TODO: copy = as.TYPE(copy)
##   TODO: copy_data = as.TYPE(copy_data)
##   TODO: close = as.TYPE(close)
##   TODO: close_data = as.TYPE(close_data)
##   res <- .Call("_H5Pcreate_class", parent_class, name, create, create_data, copy, copy_data, close, close_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pregister1 <- function( class, name, size, default, create, set, get, delete, copy, close ) {
##   TODO: class = as.TYPE(class)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   size = as.integer(size)
##   TODO: default = as.TYPE(default)
##   TODO: create = as.TYPE(create)
##   TODO: set = as.TYPE(set)
##   TODO: get = as.TYPE(get)
##   TODO: delete = as.TYPE(delete)
##   TODO: copy = as.TYPE(copy)
##   TODO: close = as.TYPE(close)
##   res <- .Call("_H5Pregister1", class, name, size, default, create, set, get, delete, copy, close, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pregister2 <- function( class, name, size, default, create, set, get, delete, copy, compare, close ) {
##   TODO: class = as.TYPE(class)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   size = as.integer(size)
##   TODO: default = as.TYPE(default)
##   TODO: create = as.TYPE(create)
##   TODO: set = as.TYPE(set)
##   TODO: get = as.TYPE(get)
##   TODO: delete = as.TYPE(delete)
##   TODO: copy = as.TYPE(copy)
##   TODO: compare = as.TYPE(compare)
##   TODO: close = as.TYPE(close)
##   res <- .Call("_H5Pregister2", class, name, size, default, create, set, get, delete, copy, compare, close, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pinsert1 <- function( plid, name, size, value, set, get, delete, copy, close ) {
##   TODO: plid = as.TYPE(plid)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   size = as.integer(size)
##   TODO: value = as.TYPE(value)
##   TODO: set = as.TYPE(set)
##   TODO: get = as.TYPE(get)
##   TODO: delete = as.TYPE(delete)
##   TODO: copy = as.TYPE(copy)
##   TODO: close = as.TYPE(close)
##   res <- .Call("_H5Pinsert1", plid, name, size, value, set, get, delete, copy, close, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pinsert2 <- function( plid, name, size, value, set, get, delete, copy, compare, close ) {
##   TODO: plid = as.TYPE(plid)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   size = as.integer(size)
##   TODO: value = as.TYPE(value)
##   TODO: set = as.TYPE(set)
##   TODO: get = as.TYPE(get)
##   TODO: delete = as.TYPE(delete)
##   TODO: copy = as.TYPE(copy)
##   TODO: compare = as.TYPE(compare)
##   TODO: close = as.TYPE(close)
##   res <- .Call("_H5Pinsert2", plid, name, size, value, set, get, delete, copy, compare, close, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pset <- function( plid, name, value ) {
##   TODO: plid = as.TYPE(plid)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   TODO: value = as.TYPE(value)
##   res <- .Call("_H5Pset", plid, name, value, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pexist <- function( id, name ) {
##   TODO: id = as.TYPE(id)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   res <- .Call("_H5Pexist", id, name, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_size <- function( id, name, size ) {
##   TODO: id = as.TYPE(id)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   TODO: size = as.TYPE(size)
##   res <- .Call("_H5Pget_size", id, name, size, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_nprops <- function( plist_id, nprops ) {
##   h5checktype(h5plist, "plist")
##   TODO: nprops = as.TYPE(nprops)
##   res <- .Call("_H5Pget_nprops", plist_id, nprops, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_class_name <- function( pcid ) {
##   TODO: pcid = as.TYPE(pcid)
##   res <- .Call("_H5Pget_class_name", pcid, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget_class_parent <- function( pcid ) {
##   TODO: pcid = as.TYPE(pcid)
##   res <- .Call("_H5Pget_class_parent", pcid, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pisa_class <- function( plist, pclass ) {
##   h5checktype(h5plist, "plist")
##   TODO: pclass = as.TYPE(pclass)
##   res <- .Call("_H5Pisa_class", plist, pclass, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pget <- function( plid, name, value ) {
##   TODO: plid = as.TYPE(plid)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   TODO: value = as.TYPE(value)
##   res <- .Call("_H5Pget", plid, name, value, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

H5Pequal <- function( h5plistclass1, h5plistclass2 ) {
  h5checktype(h5plistclass1, "plistclass")
  h5checktype(h5plistclass2, "plistclass")
  res <- .Call("_H5Pequal", h5plistclass1@ID, h5plistclass2@ID, PACKAGE='rhdf5')
  as.logical(res)
}

## H5Piterate <- function( id, idx, iter_func, iter_data ) {
##   TODO: id = as.TYPE(id)
##   TODO: idx = as.TYPE(idx)
##   TODO: iter_func = as.TYPE(iter_func)
##   TODO: iter_data = as.TYPE(iter_data)
##   res <- .Call("_H5Piterate", id, idx, iter_func, iter_data, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Pcopy_prop <- function( dst_id, src_id, name ) {
##   TODO: dst_id = as.TYPE(dst_id)
##   TODO: src_id = as.TYPE(src_id)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   res <- .Call("_H5Pcopy_prop", dst_id, src_id, name, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Premove <- function( plid, name ) {
##   TODO: plid = as.TYPE(plid)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   res <- .Call("_H5Premove", plid, name, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

## H5Punregister <- function( class, name ) {
##   TODO: class = as.TYPE(class)
##   if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
##   res <- .Call("_H5Punregister", class, name, PACKAGE='rhdf5')
##   SEXP Rval = R_NilValue;
##   invisible(res)
## }

H5Pclose_class <- function( h5plistclass ) {
  h5checktype(h5plistclass, "plistclass")
  invisible(.Call("_H5Pclose_class", h5plistclass@ID, PACKAGE='rhdf5'))
}

