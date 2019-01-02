
setMethod("show",signature="H5IdComponent", function(object) {
  res <- .Call("_handleInfo", object@ID, PACKAGE='rhdf5')
  res$type = h5const2String("H5I_TYPE",res$type)
  res$type = substr(res$type,5,1000)
  cat(sprintf("HDF5 %s",res$type),
      if (object@native) "(native)",
      "\n")
  if (res$type == "DATASPACE") {
    res2 <- H5Sget_simple_extent_dims(object)
    res$info = c(res$info,
      rank = res2$rank,
      size = paste(res2$size, collapse=" x "),
      maxsize = paste(res2$maxsize, collapse=" x "))
  }
  if (res$type == "DATASET") {
    s = H5Dget_space(object)
    on.exit(H5Sclose(s))
    res2 <- H5Sget_simple_extent_dims(s)
    res$info = c(res$info,
      type =  .Call("_getDatatypeName", H5Dget_type(object), PACKAGE='rhdf5'),
      rank = res2$rank,
      size = paste(res2$size, collapse=" x "),
      maxsize = paste(res2$maxsize, collapse=" x "))
  }
  if (res$type == "ATTR") {
    s = H5Aget_space(object)
    on.exit(H5Sclose(s))
    res2 <- H5Sget_simple_extent_dims(s)
    names(res$info)[names(res$info) == "name"] = "objName"
    res$info = c(res$info,
      attrName = H5Aget_name(object),
      type = .Call("_getDatatypeName", H5Aget_type(object), PACKAGE='rhdf5'),
      rank = res2$rank,
      size = paste(res2$size, collapse=" x "),
      maxsize = paste(res2$maxsize, collapse=" x "))
  }
  if (res$type == "DATATYPE") {
    res$info = c(res$info,
      type = .Call("_getDatatypeName", object@ID, PACKAGE='rhdf5'))
  }
  if (!is.null(res$info)) {
    cat(paste(sprintf("%12s %s\n",names(res$info),res$info),collapse=""))
  }
  if (res$type %in% c("FILE","GROUP")) {
    cat("\n")
    x = h5ls(object, recursive=FALSE)
    x = format(x, justify = "left")
    x = x[,-1]
    print(x)
  }
  if (!res$isvalid) {
    cat("\nNo valid identifier.\n")
  }
})

setMethod( `$`, signature = c('H5IdComponent'),
           function(x, name) {
             h5id = x
             isvalid = H5Iis_valid(h5id)
             if (!isvalid) {
               stop("Bad HDF5 ID. File, group, or dataset closed?", call. = FALSE)
             }
             truetype = H5Iget_type(h5id)
             #   par = list(file=h5id, name=name)
             #   if ( .hasSlot(h5id, "par") ) {
             #     par = c(par, h5id@par[names(h5id@par) %in% c("compoundAsDataFrame","read.attributes")])
             #   }
             if (truetype %in% c("H5I_FILE", "H5I_GROUP")) {
               res = h5read(file = h5id, name = name)
             } else {
               stop("The provided H5Identifier is not a location identifier.", 
                    call. = FALSE)
             }
             #   
             #   else {
             #     if (truetype == "H5I_DATASET") {
             #       h5a = H5Aopen(h5obj = h5id, name=)
             #       res = h5readAttributes(h5id, name = name)
             #     } else {
             #       stop("The provided H5Identifier is neither a location identifier nor a dataset identifier.", 
             #            call. = FALSE)
             #     }
             #   }
             res
           } )

setMethod(`&`, signature = c("H5IdComponent"), 
          function(e1, e2) {
            h5id = e1
            name = e2
            H5Oopen(h5id, name)
          } )

setMethod(`[`, signature = c("H5IdComponent", "ANY", "ANY", "ANY"),
          function(x, i, j, ..., drop = TRUE) {
            h5id = x
            index = as.list(sys.call())[-c(1,2)]
            for (i in seq_along(index)) {
              if (index[[i]] == '') {
                index[i] = list(c())
              }
            }
            if (length(index) == 1) {
              if (is.null(index[[1]])) {
                index = NULL
              }
            }
            isvalid = H5Iis_valid(h5id)
            if (!isvalid) {
              stop("Bad HDF5 ID. Dataset closed?", call. = FALSE)
            }
            truetype = H5Iget_type(h5id)
            if (truetype == "H5I_DATASET") {
              res = h5readDataset(h5dataset = h5id, index = index)
              if (drop) {
                res = drop(res)
              }
            } else {
              stop("The provided H5Identifier is not a dataset identifier and can not be subsetted.", 
                   call. = FALSE)
            }
            res
          } )


setMethod(`[<-`, signature = c("H5IdComponent", "ANY","ANY","ANY"),
          function(x, i, j, ..., value) {
            h5id = x
            index = as.list(sys.call())
            index = index[-c(1,2,length(index))]
            for (i in seq_along(index)) {
              if (index[[i]] == '') {
                index[i] = list(c())
              }
            }
            if (length(index) == 1) {
              if (is.null(index[[1]])) {
                index = NULL
              }
            }
            isvalid = H5Iis_valid(h5id)
            if (!isvalid) {
              stop("Bad HDF5 ID. Dataset closed?", call. = FALSE)
            }
            truetype = H5Iget_type(h5id)
            if (truetype == "H5I_DATASET") {
              res = h5writeDatasetHelper(obj=value, h5dataset = h5id, index=index)
            } else {
              stop("The provided H5Identifier is not a dataset identifier and can not be subsetted.", 
                   call. = FALSE)
            }
            h5id
          } )

setMethod(`$<-`, signature = c("H5IdComponent"),
          function(x, name, value) {
            h5id = x
            isvalid = H5Iis_valid(h5id)
            if (!isvalid) {
              stop("Bad HDF5 ID. File, group, or dataset closed?", call. = FALSE)
            }
            truetype = H5Iget_type(h5id)
            if (truetype %in% c("H5I_FILE", "H5I_GROUP")) {
              res = h5write(value, file = h5id, name = name)
            } else {
              stop("The provided H5Identifier is not a location identifier.", 
                   call. = FALSE)
            }
            h5id
          } )

