#' Delete objects within a HDF5 file
#'
#' Deletes the specified group or dataset from within an HDF5 file.
#'
#' @param file The filename (character) of the file in which the object is
#' located.
#' @param name For `h5delete` the name of the object to be deleted. For
#' `h5deleteAttribute` the name of the object to which the attribute
#' belongs.
#' @author Mike Smith
#' @name h5_delete
#' @export h5delete
#' @examples
#' h5File <- tempfile(pattern = "ex_createFile.h5")
#'
#' h5createFile(h5File)
#' h5createGroup(h5File, "foo")
#'
#' h5ls(h5File)
#'
#' h5delete(h5File, "foo")
#' h5ls(h5File)
#'
h5delete <- function(file, name) {
  loc <- h5checktypeOrOpenLoc(file, native = FALSE)
  on.exit(h5closeitLoc(loc))

  H5Ldelete(h5loc = loc$H5Identifier, name = name)
}

#' Delete attribute
#'
#' Deletes an attribute associated with a group or dataset within an HDF5 file.
#'
#' @param file The filename (character) of the file in which the object is
#' located.
#' @param name The name of the object to which the attribute belongs.
#' @param attribute Name of the attribute to be deleted.
#'
#' @author Mike Smith
#' @name h5_deleteAttribute
#' @export h5deleteAttribute
#' @examples
#' h5File <- tempfile(pattern = "ex_createAttribute.h5")
#' h5createFile(h5File)
#' h5write(1:1, h5File, "A")
#' fid <- H5Fopen(h5File)
#' did <- H5Dopen(fid, "A")
#' h5createAttribute(did, "time", c(1, 10))
#' h5readAttributes(h5File, "A")
#' h5deleteAttribute(h5File, "A", "time")
#' h5readAttributes(h5File, "A")
#' H5Dclose(did)
#' H5Fclose(fid)
#'
h5deleteAttribute <- function(file, name, attribute) {
  if (missing(name)) {
    stop(
      "Please provide the name of the object to which the attribute belongs ",
      "using the 'name' argument. You can use `h5ls()` to list the objects in ",
      "the file",
      call. = FALSE
    )
  }
  loc <- h5checktypeOrOpenLoc(file, native = FALSE)
  on.exit(h5closeitLoc(loc))

  res <- FALSE
  if (H5Lexists(loc$H5Identifier, name)) {
    oid <- H5Oopen(loc$H5Identifier, name = name)
    on.exit(H5Oclose(oid), add = TRUE)
    if (H5Aexists(h5obj = oid, name = attribute)) {
      res <- !(H5Adelete(h5obj = oid, name = attribute))
    } else {
      message("Attribute '", attribute, "' not found.")
    }
  } else {
    message(
      "Object '",
      name,
      "' not found in ",
      file,
      ".\n",
      "Use `h5ls()` to list the objects in the file."
    )
  }
  return(invisible(res))
}
