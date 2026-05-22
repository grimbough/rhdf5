# Set the read-only S3 virtual file driver

The read-only S3 virtual file driver can be used to read files hosted
remotely on Amazon's S3 storage.

## Usage

``` r
H5Pset_fapl_ros3(h5plist, s3credentials = NULL)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a file access property list.

- s3credentials:

  Either `NULL` or a list of length 3 specifying the AWS access
  credentials (see details).

## Details

To access files in a private Amazon S3 bucket you will need to provide
three additional details: The AWS region where the files are hosted,
your AWS access key ID, and your AWS secret access key. More information
on how to obtain AWS access keys can be found at
<https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html#access-keys-and-secret-access-keys>.
These are provided as a list to the `s3credentials` argument. If you are
accessing public data this argument should be `NULL`.

## Examples

``` r

## this doesn't work on the Bioconductor Mac build machine
if (FALSE) { # \dontrun{
pid <- H5Pcreate("H5P_FILE_ACCESS")
H5Pset_fapl_ros3(pid)
H5Pclose(pid)
} # }
```
