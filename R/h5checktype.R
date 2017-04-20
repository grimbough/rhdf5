h5checktype <- function(h5id, type, fctname = deparse(match.call()[1]), allow.character = FALSE) {
  if (!is( h5id, "H5IdComponent" ) ) {
    if (allow.character) {
      stop("Error in ", fctname, ". Argument neither of class H5IdComponent nor a character.", call. = FALSE)
    } else {
      stop("Error in ", fctname, ". Argument not of class H5IdComponent.", call. = FALSE)
    }
  }
  isvalid = H5Iis_valid(h5id)
  if (!isvalid) {
    stop("Error in ", fctname, ". H5Identifier not valid.", call. = FALSE)
  }
  truetype = H5Iget_type(h5id)
  switch(type,
         file = {
           if (truetype != "H5I_FILE") {
             stop("Error in ", fctname, ". The provided H5Identifier is not a file identifier.", call. = FALSE)
           }
           0
         },
         group = {
           if (truetype != c("H5I_GROUP")) {
             stop("Error in ", fctname, ". The provided H5Identifier is not a group identifier.", call. = FALSE)
           }
           0
         },
         loc = {
           if (!(truetype %in% c("H5I_FILE","H5I_GROUP"))) {
             stop("Error in ", fctname, ". The provided H5Identifier is not a location identifier.", call. = FALSE)
           }
           0
         },
         dataset = {
           if (truetype != "H5I_DATASET") {
             stop("Error in ", fctname, ". The provided H5Identifier is not a dataset identifier.", call. = FALSE)
           }
           0
         },
         object = {
           if (!(truetype %in% c("H5I_FILE","H5I_GROUP","H5I_DATASET"))) {
             stop("Error in ", fctname, ". The provided H5Identifier is not an object identifier.", call. = FALSE)
           }
           0
         },
         dataspace = {
           if (truetype != "H5I_DATASPACE") {
             stop("Error in ", fctname, ". The provided H5Identifier is not a dataspace identifier.", call. = FALSE)
           }
           0
         },
         attribute = {
           if (truetype != "H5I_ATTR") {
             stop("Error in ", fctname, ". The provided H5Identifier is not an attribute identifier.", call. = FALSE)
           }
           0
         },
         type = {
           if (truetype != "H5I_DATATYPE") {
             stop("Error in ", fctname, ". The provided H5Identifier is not a type identifier.", call. = FALSE)
           }
           0
         },
         plist = {
           if (truetype != "H5I_GENPROP_LST") {
             stop("Error in ", fctname, ". The provided H5Identifier is not a property list.", call. = FALSE)
           }
           0
         },
         plistclass = {
           if (truetype != "H5I_GENPROP_CLS") {
             stop("Error in ", fctname, ". The provided H5Identifier is not a property list class.", call. = FALSE)
           }
           0
         },
         {
           stop("argument for type unknown")
         }
       )
  return(invisible(NULL))
}

h5checktypeOrNULL <- function(h5id, type, fctname = deparse(match.call()[1])) {
  if (!is.null(h5id)) {
    h5checktype(h5id, type, fctname = fctname)
  }
  invisible(NULL)
}

h5FileIsOpen <- function(filename) {
  filename = normalizePath(filename,mustWork = FALSE)
  L = h5validObjects()
  isobject = sapply(L, function(x) {
      H5Iget_type(x) %in% c("H5I_FILE","H5I_GROUP","H5I_DATASET")
  } )
  if (length(isobject) > 0) {
    isopen = any(sapply(L[which(isobject)], function(x) {
      H5Fget_name(x) == filename
    } ))
  } else {
    isopen = FALSE
  }
  isopen
}

h5checktypeOrOpenLoc <- function(file, fctname = deparse(match.call()[1]), createnewfile=FALSE, readonly=FALSE) {
  res = list()
  if (is.character(file)) {
    file = normalizePath(file,mustWork = FALSE)
    if (file.exists(file)) {
      if (h5FileIsOpen(file)) {
        warning("An open HDF5 file handle exists. If the file has changed on disk meanwhile, the function may not work properly. Run 'H5close()' to close all open HDF5 object handles.")
      }
      
      h5loc <- if (readonly) {
          H5Fopen(file,"H5F_ACC_RDONLY")
        } else {
          H5Fopen(file)
        }
      if (!is(h5loc, "H5IdComponent")) {
        stop("Error in ",fctname,". File '",file,"' is not a valid HDF5 file.")
      } else {
        res$H5Identifier = h5loc
        res$closeit = TRUE
      }
    } else {
      if (createnewfile) {
        h5loc <- H5Fcreate(file)
        if (!is(h5loc, "H5IdComponent")) {
          stop("Error in ",fctname,". Cannot create file.")
        } else {
          res$H5Identifier = h5loc
          res$closeit = FALSE
        }
      } else {
        stop("Error in ",fctname,". Cannot open file. File '",file,"' does not exist.")
      }
    }
  } else {
    h5checktype(file, "loc", fctname = fctname, allow.character = TRUE)
    res$H5Identifier = file
    res$closeit = FALSE
  }
  invisible(res)
}

h5closeitLoc <- function(file, fctname = deparse(match.call()[1])) {
  res = TRUE
  if (file$closeit) {
    if (H5Iis_valid(file$H5Identifier)) {
      res = H5Fclose(file$H5Identifier)
    } else {
      res = FALSE
    }
  } else {
    res = FALSE
  }
  invisible(res)
}

h5checktypeOrOpenObj <- function(obj, file, fctname = deparse(match.call()[1])) {
  res = list()
  if (is.character(obj)) {
    if (missing(file)) {
      stop("Error in ",fctname,". If the object is specified by name, then you have to specify the file.")
    }
    loc = h5checktypeOrOpenLoc(file, fctname=fctname)

    if (!H5Lexists(loc$H5Identifier, obj)) {
      stop("Error in ",fctname,". Object '",obj,"' not found in file.")
    } else {
      h5obj = H5Oopen(loc$H5Identifier, obj)
      if (!is(h5obj, "H5IdComponent")) {
        stop("Error in ",fctname,". Cannot open object.")
      } else {
        res$H5Identifier = h5obj
        res$closeit = TRUE
      }
    }
    h5closeitLoc(loc)   
  } else {
    h5checktype(obj, "object", fctname = fctname, allow.character = TRUE)
    res$H5Identifier = obj
    res$closeit = FALSE
  }
  invisible(res)
}

h5closeitObj <- function(obj, fctname = deparse(match.call()[1])) {
  res = TRUE
  if (obj$closeit) {
    if (H5Iis_valid(obj$H5Identifier)) {
      res = H5Oclose(obj$H5Identifier)
    } else {
      res = FALSE
    }
  } else {
    res = FALSE
  }
  invisible(res)
}

h5checktypeAndPLC <- function(h5id, plc, allowNULL = FALSE, fctname = deparse(match.call()[1])) {
  if (is.null(h5id)) {
    if (!allowNULL) {
      stop("Error in ", fctname, ". Property list is null", call. = FALSE)
    } else {
      h5id = new("H5IdComponent", ID = integer(0))
    }
  } else {
    if (!is( h5id, "H5IdComponent" ) ) {
      stop("Error in ", fctname, ". Argument not of class H5IdComponent.", call. = FALSE)
    }
    isvalid = H5Iis_valid(h5id)
    if (!isvalid) {
      stop("Error in ", fctname, ". H5Identifier not valid.", call. = FALSE)
    }
    truetype = H5Iget_type(h5id)
    if (truetype != "H5I_GENPROP_LST") {
      stop("Error in ", fctname, ". The provided H5Identifier is not a property list.", call. = FALSE)
    }
    h5plc = H5Pget_class(h5id)
    if (!(plc %in% names(h5constants[["H5P"]]))) {
      stop("plist class '",plc,"' unknown")
    }
    if (!.Call("_H5Pequal", h5plc@ID, h5constants[["H5P"]][plc], PACKAGE='rhdf5')) {
      stop("property list is not of class '",plc,"'")
    }
    H5Pclose_class(h5plc)
  }
  return(invisible(h5id))
}

