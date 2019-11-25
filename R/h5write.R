

h5writeDatasetHelper <- function (obj, h5dataset, index = NULL, start = NULL, stride = NULL, 
                                  block = NULL, count = NULL)
{
    try({
        h5spaceFile <- H5Dget_space(h5dataset)
        on.exit(H5Sclose(h5spaceFile))
    })
    
    if (!is.null(index)) {
        s = H5Sget_simple_extent_dims(h5spaceFile)$size
        if (length(index) != length(s)) {
            stop("length of index has to be equal to dimensional extension of HDF5 dataset.")
        }
        for (i in seq_len(length(index))) {
            if (is.null(index[[i]])) {
                index[[i]] = seq_len(s[i])
            } else if( is.call(index[[i]] )) {
                index[[i]] <- eval(index[[i]])
            }
        }
        try({
            H5Sselect_index(h5spaceFile, index)
        })
        if(length(index) > 1) {
            ## indexing an array
            d = sapply(index, length)
            d[d == 0] = dim(obj)[d == 0]
            dim(obj) = d
        } 
        I = list()
        for (i in seq_len(length(index))) {
            m <- match(index[[i]], unique(sort(index[[i]])))
            I[[i]] = order(m)
            I[[i]] = I[[i]][!duplicated(m[I[[i]]], fromLast = TRUE)]
        }
        obj <- do.call("[", c(list(obj), I))
    }
    else {
        if (any(c(!is.null(start), !is.null(stride), !is.null(block), 
                  !is.null(count)))) {
            if (is.null(block) & is.null(count)) {
                if(is.null(dim(obj))) {
                    block <- 1
                    count <- 1
                } else {
                    block = rep(1, length(dim(obj)))
                    count = dim(obj)
                }
            }
            try({
                H5Sselect_hyperslab(h5spaceFile, start = start, 
                                    stride = stride, count = count, block = block)
            })
        }
    }
    DimMem <- dim(obj)
    if (is.null(DimMem)) {
        DimMem = length(obj)
    }
    try({
        h5spaceMem <- H5Screate_simple(DimMem, NULL)
        on.exit(H5Sclose(h5spaceMem), add = TRUE, after = FALSE)
    })
    try({
        res <- H5Dwrite(h5dataset, obj, h5spaceMem = h5spaceMem, 
                        h5spaceFile = h5spaceFile)
    })

    invisible(NULL)
}


h5write <- function(obj, file, name, ...) {
    res <- UseMethod("h5write")
    invisible(res)
}

h5write.default <- function(obj, file, name, createnewfile=TRUE, write.attributes = FALSE, ..., native = FALSE) {
    loc = h5checktypeOrOpenLoc(file, createnewfile = createnewfile, native = native)
    on.exit(h5closeitLoc(loc))
    
    res <- h5writeDataset(obj, loc$H5Identifier, name, ...)
    if (write.attributes) {
        # type = H5Oget_info_by_name(loc$H5Identifier, name)$type
        oid = H5Oopen(loc$H5Identifier, name)
        type = H5Iget_type(oid)
        H5Oclose(oid)
        
        if (type == "H5I_GROUP") {
            h5obj = H5Gopen(loc$H5Identifier, name)
            on.exit(H5Gclose(h5obj), add = TRUE)
        } else {
            if (type == "H5I_DATASET") {
                h5obj = H5Dopen(loc$H5Identifier, name)
                on.exit(H5Dclose(h5obj), add = TRUE)
            } else {
                stop("Cannot open object of this type")
            }
        }
        Attr <- attributes(obj)
        for (i in seq_len(length(Attr))) {
            h5writeAttribute(Attr[[i]], h5obj, name = names(Attr)[i])
        }
    }
    invisible(res)
}

h5writeDataset <- function(obj, h5loc, name, ...) {
    h5checktype(h5loc, "loc")
    res <- UseMethod("h5writeDataset")
    invisible(res)
}

h5writeDataset.data.frame <- function(obj, h5loc, name, level=7, chunk, DataFrameAsCompound = TRUE) {
    if (DataFrameAsCompound) {
        if (H5Lexists(h5loc, name)) {
            stop("Cannot write data.frame. Object already exists. Subsetting for compound datatype not supported.")
        }
        if (!is.null(level)) { 
            level = as.integer(level)
            if(missing(chunk)) 
                chunk <- nrow(obj)
        }
        did <- .Call("_h5createDataFrame", obj, h5loc@ID, name, level, as.integer(chunk), PACKAGE='rhdf5')
        .Call("_h5writeDataFrame", obj, did, PACKAGE='rhdf5')
        .Call("_H5Dclose", did, PACKAGE='rhdf5')
        res <- 0
    } else {
        a <- attr(obj,"names")
        if (is.null(a)) {
            attr(obj,"names") = sprintf("col%d",seq_len(ncol(obj)))
        } else {
            if (any(duplicated(a))) {
                a[duplicated(a)] = sprintf("col%d",seq_len(ncol(obj)))[duplicated(a)]
                attr(obj,"names") = a
            }
        }
        ## we can't write out factors, so convert any to character
        colClass <- sapply(obj, is.factor)
        if(any(colClass)) {
            #obj[,which(colClass)] <- as.character(obj[,which(colClass)])
            obj[ which(colClass) ] <- lapply(obj[ ,which(colClass), drop=FALSE], FUN = as.character)
        }
        
        res <- h5writeDataset.list(obj=obj, h5loc=h5loc, name=name, level=level)
    }
    invisible(res)
}

h5writeDataset.list <- function(obj, h5loc, name, level=7) {
    exists <- try( { H5Lexists(h5loc, name) } )
    if (exists) {
        message("Existing object within HDF5 file cannot be overwritten with a list object. First delete the group or dataset from the HDF5 file.")
        res = 0
    } else {
        N = names(obj)
        newnames = FALSE
        if (is.null(N)) {
            newnames = TRUE
        } else {
            if (any(nchar(N) == 0)) {
                newnames = TRUE
            } else {
                if (length(N) != length(obj)) {
                    newnames = TRUE
                }
            }
        }
        if (newnames) {
            N = sprintf("ELT%d",seq_len(length(obj)))
        }
        res = NULL
        h5createGroup(h5loc, name)
        gid = H5Gopen(h5loc, name)
        for (i in seq_len(length(obj))) {
            res = h5write(obj[[i]], gid, N[i])
        }
        H5Gclose(gid)
    }
}

h5writeDataset.matrix <- function(...) { h5writeDataset.array(...) }
h5writeDataset.integer <- function(...) { h5writeDataset.array(...) }
h5writeDataset.double <- function(...) { h5writeDataset.array(...) }
h5writeDataset.logical <- function(...) { h5writeDataset.array(...) }
h5writeDataset.character <- function(...) { h5writeDataset.array(...) }

h5writeDataset.array <- function(obj, h5loc, name, index = NULL, start=NULL, stride=NULL, block=NULL, count=NULL, size=NULL, level=7) {

    exists <- try( { H5Lexists(h5loc, name) } )
    if (!exists) {
        if (is.null(size)) {
            size = NULL
            if (storage.mode(obj) == "character") {
                if (length(obj) > 0) {
                    size <- max(nchar(obj), na.rm = TRUE)+1
                    ## if any NA, the minimum string length is 3
                    if(any(is.na(obj)) && size < 3) { size <- 3 }
                } else {
                    size <- 1
                }
            }
            if (is.null(dim(obj))) {
                dim <- length(obj) 
            } else {
                dim <- dim(obj)
                if (h5loc@native) dim <- rev(dim)
            }
            h5createDataset(h5loc, name, dim, storage.mode = storage.mode(obj), 
                            size = size, chunk=dim, level=level) 
        }
    }
    h5dataset <- H5Dopen(h5loc, name)
    on.exit( H5Dclose(h5dataset) )
    h5writeDatasetHelper(obj=obj, h5dataset=h5dataset, index = index, start = start, stride = stride, 
                         block = block, count = count)
    if(storage.mode(obj) == "character" && any(is.na(obj))) {
        h5writeAttribute(1L, h5dataset, name = "as.na")
        if(any(obj == "NA", na.rm = TRUE)) {
            warning("Both NA_character_ and the string 'NA' detected.\n",
                    "These will all be coerced to NA_character_ when read using h5read()")
        }
    }

    invisible(NULL)
}


