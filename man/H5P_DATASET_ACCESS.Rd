\name{HDF5 File Access Property List Interface}
\alias{H5P_FILE_ACCESS}
\alias{H5Pset_fapl_ros3}
\title{HDF5 File Access Property List Interface}
\description{The functions, macros, and subroutines listed here are used to 
    manipulate file access property lists (\code{fapl}) objects in various 
    ways.}
\usage{
H5Pset_fapl_ros3  ( h5plist, s3credentials = NULL)
}
\arguments{
  \item{h5plist}{An object of class \code{\link{H5IdComponent}} representing a H5 property list identifier of class H5P_FILE_ACCESS. See \code{\link{H5Pcreate}} or \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{s3credentials}{Either \code{NULL} or a list of length 3 specifying the AWS access credentials (see details).}
}
\details{
To access files in a private Amazon S3 bucket you will need to provide three 
additional details: The AWS region where the files are hosted, your AWS access 
key ID, and your AWS secret access key.  More information on how to obtain AWS
access keys can be found at \url{https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html#access-keys-and-secret-access-keys}.
These are provided as a list to the \code{s3credentials} argument.  If you 
are accessing public data this argument should be \code{NULL}.

See \url{https://portal.hdfgroup.org/display/HDF5/File+Access+Properties} for 
further details. See \link{H5P} for documentation of more functions to 
manupulate property lists of other property list classes. 
}

\value{
The functions return the standard return value from their respective C-functions.
}

\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Mike L. Smith}
\seealso{
    \link{H5P}
}
\examples{

## creating a fapl for accessing a public S3 bucket
\donttest{
pid <- H5Pcreate("H5P_FILE_ACCESS")
H5Pset_fapl_ros3( pid )
H5Pclose(pid)
}

## creating a fapl for accessing a private S3 bucket
## and provding required S3 credentials list

## these are example credentials and will not provide access
s3_cred <- list(
    aws_region = "eu-central-1",
    access_key_id = "AKIAIOSFODNN7EXAMPLE",
    secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
)

\donttest{
pid <- H5Pcreate("H5P_FILE_ACCESS")
H5Pset_fapl_ros3( pid, s3credentials = s3_cred )
H5Pclose(pid)
}

}
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
