
H5Lcreate_external <- function( target_file_name, target_obj_name, link_loc, link_name) {
  if (length(target_file_name)!=1 || !is.character(target_file_name)) stop("'target_file_name' must be a character string of length 1")
  target_file_name = normalizePath(target_file_name,mustWork = FALSE)
  if (length(target_obj_name)!=1 || !is.character(target_obj_name)) stop("'target_obj_name' must be a character string of length 1")
  h5checktype( link_loc, "loc")
  if (length(link_name)!=1 || !is.character(link_name)) stop("'link_name' must be a character string of length 1")

  invisible(.Call("_H5Lcreate_external", target_file_name, target_obj_name, link_loc@ID, link_name, PACKAGE='rhdf5'))
}

H5Lexists <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  name = strsplit(name,split="/")[[1]]
  name = name[nchar(name) > 0]
  Lexists = TRUE
  i=1
  while ((i <= length(name)) & (Lexists)) {
    res <- .Call("_H5Lexists", h5loc@ID, paste(name[1:i],collapse="/"), PACKAGE='rhdf5')
    Lexists <- ifelse(res > 0, TRUE, FALSE)
    i <- i + 1
  }
  Lexists
}

H5Lget_info <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  res <- .Call("_H5Lget_info", h5loc@ID, name, PACKAGE='rhdf5')
  res$type <- h5const2String("H5L_TYPE", res$type)
  res
}

H5Ldelete <- function( h5loc, name ) {
    
    h5checktype(h5loc, "loc")
    if ( length(name) != 1 || !is.character(name) ) {
        stop("'name' must be a character string of length 1")
    }
    
    if(!H5Lexists(h5loc, name)) {
        stop("Specified link doesn't exist.")
    }
    
    res <- .Call("_H5Ldelete", h5loc@ID, name, PACKAGE='rhdf5')
    
    if(res < 0) {
        stop('Link deletion failed')
    } else {
        return( invisible(res) )
    }
}

H5Lmove <- function( h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL ) {
    
    h5checktype(h5loc, "loc")
    h5checktype(h5loc_dest, "loc")

    ## if not provided a creation property list, use the default   
    if(is.null(lcpl)) {
        lcpl <- H5Pcreate("H5P_LINK_CREATE")
        on.exit(H5Pclose(lcpl), add = TRUE)
    } else {
        lcpl <- h5checktypeAndPLC(lcpl, "H5P_LINK_CREATE", allowNULL = FALSE)
    }
    
    ## use default access property list if not given
    if(is.null(lapl)) {
        lapl <- H5Pcreate("H5P_LINK_ACCESS")
        on.exit(H5Pclose(lapl), add = TRUE)
    } else {
        lapl <- h5checktypeAndPLC(lapl, "H5P_LINK_ACCESS", allowNULL = FALSE) 
    }
    
    res <- .Call("_H5Lmove", h5loc@ID, name, h5loc_dest@ID, name_dest, lcpl, lapl, PACKAGE='rhdf5')
    
    if(res < 0) {
        stop('Link deletion failed')
    } else {
        return( invisible(res) )
    }
  
}


H5Lcopy <- function( h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL ) {
    
    h5checktype(h5loc, "loc")
    h5checktype(h5loc_dest, "loc")
    
    ## if not provided a creation property list, use the one from the source    
    if(is.null(dcpl)) {
        lcpl <- H5Pcreate("H5P_LINK_CREATE")
        on.exit(H5Pclose(lcpl), add = TRUE)
    } else {
        lcpl <- h5checktypeAndPLC(lcpl, "H5P_LINK_CREATE", allowNULL = FALSE)
    }
    
    ## use default access property list if not given
    if(is.null(lapl)) {
        lapl <- H5Pcreate("H5P_LINK_ACCESS")
        on.exit(H5Pclose(lapl), add = TRUE)
    } else {
        lapl <- h5checktypeAndPLC(lapl, "H5P_LINK_ACCESS", allowNULL = FALSE) 
    }
    
    res <- .Call("_H5Lcopy", h5loc@ID, name, h5loc_dest@ID, name_dest, lcpl, lapl, PACKAGE='rhdf5')
    
    if(res < 0) {
        stop('Link deletion failed')
    } else {
        return( invisible(res) )
    }
    
}
