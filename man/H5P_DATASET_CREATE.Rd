\name{HDF5 Dataset Create Property List Interface}
\alias{H5P_DATASET_CREATE}
\alias{H5Pset_layout}
\alias{H5Pget_layout}
\alias{H5Pset_chunk}
\alias{H5Pget_chunk}
\alias{H5Pset_deflate}
\alias{H5Pset_fill_value}
\alias{H5Pget_fill_value}
\alias{H5Pfill_value_defined}
\alias{H5Pset_fill_time}
\alias{H5Pget_fill_time}
\alias{H5Pset_alloc_time}
\alias{H5Pget_alloc_time}
\alias{H5Pset_filter}
\alias{H5Pall_filters_avail}
\alias{H5Pget_nfilters}
\alias{H5Pget_filter1}
\alias{H5Pget_filter2}
\alias{H5Pget_filter_by_id1}
\alias{H5Pget_filter_by_id2}
\alias{H5Pmodify_filter}
\alias{H5Premove_filter}
\alias{H5Pset_fletcher32}
\alias{H5Pset_nbit}
\alias{H5Pset_scaleoffset}
\alias{H5Pset_shuffle}
\alias{H5Pset_szip}
\alias{H5Pset_bzip2}
\alias{H5Pset_blosc}
\alias{H5Pset_lzf}
\alias{H5Pset_external}
\alias{H5Pget_external_count}
\alias{H5Pget_external}
\title{HDF5 Dataset Create Property List Interface}
\description{The functions, macros, and subroutines listed here are used to manipulate dataset creation property list objects in various ways, including to reset property values. With the use of property lists, HDF5 functions have been implemented and can be used in applications with many fewer parameters than would be required without property lists.}
\usage{
H5Pset_layout              ( h5plist, layout = h5default("H5D") )
H5Pget_layout              ( h5plist )
H5Pset_chunk               ( h5plist, dim )
H5Pget_chunk               ( h5plist )
H5Pset_deflate             ( h5plist, level )
H5Pset_fill_value          ( h5plist, value )
% H5Pget_fill_value                  ( plist_id, type_id, value )
H5Pfill_value_defined      ( h5plist )
H5Pset_fill_time           ( h5plist, fill_time = h5default("H5D_FILL_TIME") )
H5Pget_fill_time           ( h5plist )
H5Pset_alloc_time          ( h5plist, alloc_time = h5default("H5D_ALLOC_TIME") )
H5Pget_alloc_time          ( h5plist )
% H5Pset_filter                      ( plist_id, filter_id, flags, cd_nelmts, cd_values[] )
H5Pall_filters_avail       ( h5plist )
% H5Pget_nfilters                    ( plist )
% H5Pget_filter1                     ( plist_id, idx, flags, cd_nelmts, cd_values, namelen, name[] )
% H5Pget_filter2                     ( plist_id, idx, flags, cd_nelmts, cd_values[], namelen, name[], filter_config )
% H5Pget_filter_by_id1               ( plist_id, filter_id, flags, cd_nelmts, cd_values[], namelen, name[] )
% H5Pget_filter_by_id2               ( plist_id, filter_id, flags, cd_nelmts, cd_values[], namelen, name[], filter_config )
% H5Pmodify_filter                   ( plist_id, filter_id, flags, cd_nelmts, cd_values[] )
% H5Premove_filter                   ( plist_id, filter )
% H5Pset_fletcher32                  ( plist_id )
% H5Pset_nbit                        ( plist_id )
% H5Pset_scaleoffset                 ( plist_id, scale_type, scale_factor )
H5Pset_shuffle             ( h5plist )
H5Pset_szip                ( h5plist, options_mask, pixels_per_block )
% H5Pset_external          ( plist, *name, offset, size )
% H5Pget_external_count    ( plist )
% H5Pget_external                    ( plist, idx, name_size, *name, *offset, *size )
H5Pset_blosc               ( h5plist, h5tid, method = 1L, level = 6L, shuffle = TRUE )
H5Pset_bzip2               ( h5plist, level = 2L )  
H5Pset_lzf                 ( h5plist, h5tid ) 
}
\arguments{
  \item{h5plist}{An object of class \code{\link{H5IdComponent}} representing a H5 property list identifier of class H5P_DATASET_CREATE. See \code{\link{H5Pcreate}} or \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{layout}{A character name of a dataset layout type. See \code{h5const("H5D")} for possible property list types. Can also be an integer representing an HDF5 dataset layout type.}
  \item{dim}{The chunk size used to store the dataset. It is an integer vector of the same length as the dataset \code{dims}.}
  \item{level}{The compression level used. An integer value between 0 (no compression) and 9 (highest and slowest compression).}
  \item{value}{Standard value for filling the dataset. The storage.mode of value has to be convertable to the dataset type by HDF5.}
  \item{fill_time}{A character name of a H5D_FILL_TIME type. See \code{h5const("H5D_FILL_TIME")} for possible values. Can also be an integer representing an HDF5 H5D_FILL_TIME type.}
  \item{alloc_time}{A character name of a H5D_ALLOC_TIME type. See \code{h5const("H5D_ALLOC_TIME")} for possible property list types. Can also be an integer representing an HDF5 H5D_ALLOC_TIME type.}
  \item{h5tid}{HDF5 type ID.}
  \item{method}{When using the BLOSC filter, defines which compression tool should be used.}
  \item{shuffle}{Logical value determining whether byte shuffling should be applied before compression is applied. This only applies to the BLOSC filter.}
  \item{options_mask}{See \url{https://portal.hdfgroup.org/display/HDF5/H5P_SET_SZIP} for more details.}
  \item{pixels_per_block}{See \url{https://portal.hdfgroup.org/display/HDF5/H5P_SET_SZIP} for more details.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{https://portal.hdfgroup.org/display/HDF5/Dataset+Creation+Properties} for further details. See \link{H5P} for documentation of more functions to manupulate property lists of other property list classes. 
}
\value{
The functions return the standard return value from their respective C-functions.
}
\references{
  \url{https://portal.hdfgroup.org/display/HDF5},
  \url{https://portal.hdfgroup.org/display/HDF5/Szip+Compression+in+HDF+Products}
}
\author{Bernd Fischer, Mike L. Smith}
\seealso{
\link{rhdf5}, \link{H5P}
}
\examples{
pid <- H5Pcreate("H5P_DATASET_CREATE")
H5Pset_fill_time( pid, "H5D_FILL_TIME_ALLOC" )
H5Pset_chunk(pid, c(1000,1,1))
H5Pset_deflate(pid, 6)

H5Pget_layout( pid )
H5Pfill_value_defined( pid )
H5Pget_fill_time( pid )
H5Pget_alloc_time( pid )
H5Pget_chunk( pid)

H5Pclose(pid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
