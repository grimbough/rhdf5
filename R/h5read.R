h5readDataset <- function (h5dataset, index = NULL, start = NULL, stride = NULL, 
                           block = NULL, count = NULL, compoundAsDataFrame = TRUE, drop = FALSE, ...) {

    h5spaceFile <- H5Dget_space(h5dataset)
    on.exit(H5Sclose(h5spaceFile))
    h5spaceMem = NULL
    if (!is.null(index)) {
        s <- H5Sget_simple_extent_dims(h5spaceFile)$size
        if (length(index) != length(s)) {
            stop("length of index has to be equal to dimensional extension of HDF5 dataset.")
        }
        
        index_null <- sapply(index, is.null)
        
        for (i in seq_along(index)) {
            if ( is.name(index[[i]]) | is.call(index[[i]]) ) {
                index[[i]] <- eval(index[[i]])  
            }
        }
        size <- .H5Sselect_dim( h5spaceFile, index)
        h5spaceMem = H5Screate_simple(size, native = h5dataset@native)
        on.exit(H5Sclose(h5spaceMem), add = TRUE)
    }
    else {
        if (any(c(!is.null(start), !is.null(stride), 
                  !is.null(count), !is.null(block)))) {
            size = 0
            try({
                size = H5Sselect_hyperslab(h5spaceFile, 
                                           start = start, stride = stride, count = count, 
                                           block = block)
            })
            h5spaceMem = H5Screate_simple(size, native = h5dataset@native)
            on.exit(H5Sclose(h5spaceMem), add = TRUE)
        }
    }
    obj <- NULL
    tryCatch({
        obj <- H5Dread(h5dataset = h5dataset, h5spaceFile = h5spaceFile, 
                       h5spaceMem = h5spaceMem,
                       compoundAsDataFrame = compoundAsDataFrame, drop = drop, ...)
    },
        error = function(e) { 
            err <- h5checkFilters(h5dataset)
            ## if we fail here it doesn't make it to the usual H5Dclose call
            on.exit(H5Dclose(h5dataset))
            if(nchar(err) > 0)
                stop(err, call. = FALSE)
            else 
                stop(e)
        }
    )

    ## Here we reorder data to match the order requested in index.
    ## The calls to H5Sselect_index will have returned data linearly
    ## from the file, not the potentially random order requested.
    if (!is.null(index)) {
        I = list()
        for (i in seq_along(index)) {
            if(!index_null[i]) { ## skip if the index was generated inside this function
              tmp <- unique(index[[i]])
              if(is.unsorted(tmp)) { 
                tmp <- sort.int(tmp) 
              } 
              I[[i]] = match(index[[i]], tmp)
            } else {
              I[[i]] <- seq_len(s[i])
            }
        }
        obj.dim <- lapply(dim(obj), FUN = seq_len)
        ## only need to compare the dimensions not set automatically
        if(!identical(I[!index_null], obj.dim[!index_null])) {
            obj <- do.call("[", c(list(obj), I, drop = FALSE))
        } 
    }

    return(obj)
}

h5read <- function(file, name, index=NULL, start=NULL, stride=NULL, block=NULL,
                   count = NULL, compoundAsDataFrame = TRUE, callGeneric = TRUE,
                   read.attributes=FALSE, drop = FALSE, ..., native = FALSE) {
    
    loc = h5checktypeOrOpenLoc(file, readonly=TRUE, native = native)
    on.exit( h5closeitLoc(loc) )
    
    if (!H5Lexists(loc$H5Identifier, name)) {
        stop("Object '", name, "' does not exist in this HDF5 file.")
    } else {
        oid = H5Oopen(loc$H5Identifier, name)
        type = H5Iget_type(oid)
        num_attrs = H5Oget_num_attrs(oid)
        if (is.na(num_attrs)) { num_attrs = 0 }
        H5Oclose(oid)
        if (type == "H5I_GROUP") {
            gid <- H5Gopen(loc$H5Identifier, name)
            obj = h5dump(gid, start=start, stride=stride, block=block, 
                         count=count, compoundAsDataFrame = compoundAsDataFrame, callGeneric = callGeneric, ...)
            H5Gclose(gid)
        } else if (type == "H5I_DATASET") {
            h5dataset <- H5Dopen(loc$H5Identifier, name)
            obj <- h5readDataset(h5dataset, index = index, start = start, stride = stride, 
                                 block = block, count = count, compoundAsDataFrame = compoundAsDataFrame, drop = drop, ...)
            ## coerce the string "NA" to NA if required
            if(storage.mode(obj) == "character" && H5Aexists(h5dataset, name = "as.na")) {
                if(any(obj == "NA")) {
                    obj[obj == "NA"] <- NA_character_
                }
            }

            ## Add back the dimnames. 
            dimnames <- vector("list", length(dim(obj)))
            for (i in seq_along(dimnames)) {
                target <- paste0("dimnames", i)
                if (H5Aexists(h5dataset, target)) { 
                    thing <- H5Aopen(h5dataset, target)
                    dimnames[[i]] <- as.character(H5Aread(thing))
                    H5Aclose(thing)
                }
            }
            dimnames(obj) <- .choose_dimnames(dim(obj), dimnames, index=index, start=start, 
                                              stride=stride, block=block, count=count)

            try( { H5Dclose(h5dataset) } )

            cl <- attr(obj,"class")
            if (!is.null(cl) & callGeneric) {
                if (exists(paste("h5read",cl,sep="."),mode="function")) {
                    obj <- do.call(paste("h5read",cl,sep="."), args=list(obj = obj))
                }
            }
        } else {
            message("Reading of object type not supported.")
            obj <- NULL
        } ## GROUP
        if (read.attributes & (num_attrs > 0) & !is.null(obj)) {
            for (i in seq_len(num_attrs)) {
                A = H5Aopen_by_idx(loc$H5Identifier, n = i-1, objname = name)
                attrname <- H5Aget_name(A)
                if (attrname != "dim") {
                    attr(obj, attrname) = H5Aread(A)
                }
                H5Aclose(A)
            }
        }
    }  # !H5Lexists
    
    obj
}

.choose_dimnames <- function(dims, dimnames, index, start, stride, block, count) {
    if (all(vapply(dimnames, is.null, FALSE))) {
        return(dimnames)
    }

    # The rest of this function duplicates h5readDataset and
    # H5Sselect_hyperslab. There must be a better way!
    if (!is.null(index)) {
        for (i in seq_along(dimnames)) {
            if (!is.null(index[[i]])) {
                dimnames[[i]] <- dimnames[[i]][index[[i]]]
            }
        }

    } else if (any(c(!is.null(start), !is.null(stride), !is.null(block), !is.null(count)))) {
        if (is.null(start)) {
            start <- rep(1L, length(dimnames))
        }
        if (is.null(stride)) {
            stride <- rep(1L, length(dimnames)) 
        }
        if (is.null(block)) {
            block <- rep(1L, length(dimnames)) 
        }
        if (is.null(count)) {
            count <- dims 
        }

        for (i in seq_along(dimnames)) {
            nblocks <- count[i]
            blocksize <- block[i]
            chosen <- outer(seq_len(blocksize), (seq_len(nblocks) - 1) * stride[i], FUN="+")
            dimnames[[i]] <- dimnames[[i]][start[i] - 1L + as.vector(chosen)]
        }
    }

    dimnames
}
