# Reading HDF5 Files In The Cloud

The *[rhdf5](https://bioconductor.org/packages/3.24/rhdf5)* provides
limited support for read-only access to HDF5 files stored in Amazon S3
buckets. This is implemented via the [HDF5 S3 Virtual File
Driver](https://portal.hdfgroup.org/display/HDF5/Virtual+File+Drivers+-+S3+and+HDFS)
and allows access to HDF5 files hosted in both public and private S3
buckets.

Currently only the functions
[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md),
[`h5dump()`](https://huber-group-embl.github.io/rhdf5/reference/h5_dump.md),
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md)
and
[`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
are supported.

``` r

library(rhdf5)
```

## Public S3 Buckets

To access a file in a public Amazon S3 bucket you provide the file’s URL
to the `file` argument. You also need to set the argument `s3 = TRUE`,
otherwise
[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md)
will treat the URL as a path on the local disk fail.

``` r

public_S3_url <- "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5"
h5ls(
  file = public_S3_url,
  s3 = TRUE
)
```

    ##   group name       otype dclass dim
    ## 0     /  DS1 H5I_DATASET  ARRAY   4

The same arguments are also valid for using
[`h5dump()`](https://huber-group-embl.github.io/rhdf5/reference/h5_dump.md)
to retrieve the contents of a file.

``` r

public_S3_url <- "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_cmpd.h5"
h5dump(
  file = public_S3_url,
  s3 = TRUE
)
```

    ## $DS1
    ##   Serial number          Location Temperature (F) Pressure (inHg)
    ## 1          1153 Exterior (static)           53.23           24.57
    ## 2          1184            Intake           55.12           22.95
    ## 3          1027   Intake manifold          103.55           31.23
    ## 4          1313  Exhaust manifold         1252.89           84.11

In addition to examining and reading whole files, we can also extract
just a subset, without needed to read or download the entire file. In
the example below we use
[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md)
to examine a file in an S3 bucket and identify the name of a dataset
within it (`a1`) and the number of dimensions for that dataset (3). We
can then use
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md)
along with the `name` and `index` arguments to read only a subset of the
dataset into our R session.

``` r

public_S3_url <- "https://rhdf5-public.s3.eu-central-1.amazonaws.com/rhdf5ex_t_float_3d.h5"
h5ls(file = public_S3_url, s3 = TRUE)
```

    ##   group name       otype dclass        dim
    ## 0     /   a1 H5I_DATASET  FLOAT 5 x 10 x 2

``` r

h5read(public_S3_url, name = "a1", index = list(1:2, 3, NULL), s3 = TRUE)
```

    ## , , 1
    ## 
    ##           [,1]
    ## [1,] 0.2444485
    ## [2,] 0.3873723
    ## 
    ## , , 2
    ## 
    ##           [,1]
    ## [1,] 0.7906603
    ## [2,] 0.3274960

## Private S3 Buckets

To access files in a private Amazon S3 bucket you will need to provide
three additional details: The AWS region where the files are hosted,
your AWS access key ID, and your AWS secret access key. More information
on how to obtain AWS access keys can be found under [AWS Security
Credentials](https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html#access-keys-and-secret-access-keys).

These three values need to be stored in a list like below. *Important
note: for now they must be in this specific order.*

``` r

## these are example credentials and will not work
s3_cred <- list(
  aws_region = "eu-central-1",
  access_key_id = "AKIAIOSFODNN7EXAMPLE",
  secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
)
```

Finally we pass this list to
[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md)
via the `s3credentials` argument.

``` r

public_S3_url <- "https://rhdf5-private.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5"
h5ls(
  file = public_S3_url,
  s3 = TRUE,
  s3credentials = s3_cred
)
```

The `s3credentials` arguments is used in exactly the same way for
[`h5dump()`](https://huber-group-embl.github.io/rhdf5/reference/h5_dump.md)
and
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md).

## Session Info

``` r

sessionInfo()
```

    ## R Under development (unstable) (2026-06-21 r90185)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] rhdf5_2.57.10    BiocStyle_2.41.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] cli_3.6.6           knitr_1.51          rlang_1.3.0        
    ##  [4] xfun_0.60           otel_0.2.0          textshaping_1.0.5  
    ##  [7] jsonlite_2.0.0      htmltools_0.5.9     ragg_1.5.2         
    ## [10] sass_0.4.10         rmarkdown_2.31      evaluate_1.0.5     
    ## [13] jquerylib_0.1.4     fastmap_1.2.0       Rhdf5lib_2.1.0     
    ## [16] yaml_2.3.12         lifecycle_1.0.5     bookdown_0.47      
    ## [19] BiocManager_1.30.27 compiler_4.7.0      fs_2.1.0           
    ## [22] rhdf5filters_1.25.4 systemfonts_1.3.2   digest_0.6.39      
    ## [25] R6_2.6.1            bslib_0.12.0        tools_4.7.0        
    ## [28] pkgdown_2.2.1       cachem_1.1.0        desc_1.4.3
