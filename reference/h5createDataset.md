# Create HDF5 dataset

R function to create an HDF5 dataset and defining its dimensionality and
compression behaviour.

## Usage

``` r
h5createDataset(
  file,
  dataset,
  dims,
  maxdims = dims,
  storage.mode = "double",
  H5type = NULL,
  size = NULL,
  encoding = NULL,
  chunk = dims,
  fillValue,
  level = 6,
  filter = "gzip",
  shuffle = TRUE,
  native = FALSE
)
```

## Arguments

- file:

  The filename (character) of the file in which the dataset will be
  located. For advanced programmers it is possible to provide an object
  of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- dataset:

  Name of the dataset to be created. The name can contain group names,
  e.g. 'group/dataset', but the function will fail, if the group does
  not yet exist.

- dims:

  The dimensions of the array as they will appear in the file. Note, the
  dimensions will appear in inverted order when viewing the file with a
  C-program (e.g. HDFView), because the fastest changing dimension in R
  is the first one, whereas the fastest changing dimension in C is the
  last one.

- maxdims:

  The maximum extension of the array. Use
  [`H5Sunlimited()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sunlimited.md)
  to indicate an extensible dimension.

- storage.mode:

  The storage mode of the data to be written. Can be obtained by
  `storage.mode(mydata)`.

- H5type:

  Advanced programmers can specify the datatype of the dataset within
  the file, either as a string with one of the available datatypes
  listed in `h5const("H5T")`, or as the output of
  [`H5Tcopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Tcopy.md).
  If `H5type` is specified the argument `storage.mode` is ignored. It is
  recommended to use `storage.mode`.

- size:

  For `storage.mode='character'` the maximum string length to use. The
  default value of `NULL` will result in using variable length strings.
  See the details for more information on this option.

- encoding:

  The encoding of the string data type. Valid options are "ASCII" or
  "UTF-8".

- chunk:

  The chunk size used to store the dataset. It is an integer vector of
  the same length as `dims`. This argument is usually set together with
  a compression property (argument `level`).

- fillValue:

  Standard value for filling the dataset. The storage.mode of value has
  to be convertible to the dataset type by HDF5.

- level:

  The compression level used. An integer value between 0 (no
  compression) and 9 (highest and slowest compression).

- filter:

  Character defining which compression filter should be applied to the
  chunks of the dataset. See the Details section for more information on
  the options that can be provided here.

- shuffle:

  Logical defining whether the byte-shuffle algorithm should be applied
  to data prior to compression.

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

## Value

Returns (invisibly) `TRUE` if dataset was created successfully and
`FALSE` otherwise.

## Details

Creates a new dataset in an existing HDF5 file. The function will fail
if the file doesn't exist or if there exists already another dataset
with the same name within the specified file.

The `size` argument is only used when `storage.mode = 'character'`. When
storing strings HDF5 can use either a fixed or variable length datatype.
Setting `size` to a positive integer will use fixed length strings where
`size` defines the length. rhdf5 writes null padded strings by default
and so to avoid data loss the value provided here should be the length
of the longest string. Setting `size = NULL` will use variable length
strings. The choice is probably dependent on the nature of the strings
you're writing. The principle difference is that a dataset of variable
length strings will not be compressed by HDF5 but each individual string
only uses the space it requires, whereas in a fixed length dataset each
string is of length uses `size`, but the whole dataset can be
compressed. This explored more in the examples below.

The `filter` argument can take several options matching to compression
filters distributed in either with the HDF5 library in Rhdf5lib or via
the rhdf5filters package. The plugins available and the corresponding
values for selecting them are shown below:

- zlib: Ubiquitous deflate compression algorithm used in GZIP or ZIP
  files. All three options below achieve the same result.:

  - `"GZIP"`,

  - `"ZLIB"`,

  - `"DEFLATE"`

- szip: Compression algorithm maintained by the HDF5 group.:

  - `"SZIP"`

- bzip2:

  - `"BZIP2"`

- BLOSC meta compressor: As a meta-compressor BLOSC wraps several
  different compression algorithms. Each of the options below will
  active a different compression filter. :

  - `"BLOSC_BLOSCLZ"`

  - `"BLOSC_LZ4"`

  - `"BLOSC_LZ4HC"`

  - `"BLOSC_SNAPPY"`

  - `"BLOSC_ZLIB"`

  - `"BLOSC_ZSTD"`

- lzf:

  - `"LZF"`

- Disable: It is possible to write chunks without any compression
  applied.:

  - `"NONE"`

## See also

[`h5createFile()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createFile.md),
[`h5createGroup()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createGroup.md),
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md),
[`h5write()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md)

## Author

Bernd Fischer, Mike L. Smith

## Examples

``` r
h5File <- tempfile(pattern = "_ex_createDataset.h5")
h5createFile(h5File)

# create dataset with compression
h5createDataset(h5File, "A", c(5, 8), storage.mode = "integer", chunk = c(5, 1), level = 6)

# create dataset without compression
h5createDataset(h5File, "B", c(5, 8), storage.mode = "integer")
h5createDataset(h5File, "C", c(5, 8), storage.mode = "double")

# create dataset with bzip2 compression
h5createDataset(h5File, "D", c(5, 8),
  storage.mode = "integer",
  chunk = c(5, 1), filter = "BZIP2", level = 6
)

# create a dataset of strings & define size based on longest string
ex_strings <- c("long", "longer", "longest")
h5createDataset(h5File, "E",
  storage.mode = "character", chunk = 3, level = 6,
  dims = length(ex_strings), size = max(nchar(ex_strings))
)


# write data to dataset
h5write(matrix(1:40, nr = 5, nc = 8), file = h5File, name = "A")
# write second column
h5write(matrix(1:5, nr = 5, nc = 1), file = h5File, name = "B", index = list(NULL, 2))
# write character vector
h5write(ex_strings, file = h5File, name = "E")

h5dump(h5File)
#> $A
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]    1    6   11   16   21   26   31   36
#> [2,]    2    7   12   17   22   27   32   37
#> [3,]    3    8   13   18   23   28   33   38
#> [4,]    4    9   14   19   24   29   34   39
#> [5,]    5   10   15   20   25   30   35   40
#> 
#> $B
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]    0    1    0    0    0    0    0    0
#> [2,]    0    2    0    0    0    0    0    0
#> [3,]    0    3    0    0    0    0    0    0
#> [4,]    0    4    0    0    0    0    0    0
#> [5,]    0    5    0    0    0    0    0    0
#> 
#> $C
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]    0    0    0    0    0    0    0    0
#> [2,]    0    0    0    0    0    0    0    0
#> [3,]    0    0    0    0    0    0    0    0
#> [4,]    0    0    0    0    0    0    0    0
#> [5,]    0    0    0    0    0    0    0    0
#> 
#> $D
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]    0    0    0    0    0    0    0    0
#> [2,]    0    0    0    0    0    0    0    0
#> [3,]    0    0    0    0    0    0    0    0
#> [4,]    0    0    0    0    0    0    0    0
#> [5,]    0    0    0    0    0    0    0    0
#> 
#> $E
#> [1] "long"    "longer"  "longest"
#> 

## Investigating fixed vs variable length string datasets

## create 1000 random strings with length between 50 and 100 characters
words <- vapply(
  X = ceiling(runif(n = 1000, min = 50, max = 100)),
  FUN = function(x) {
    paste(sample(letters, size = x, replace = TRUE),
      collapse = ""
    )
  },
  FUN.VALUE = character(1)
)

## create two HDF5 files
f1 <- tempfile()
f2 <- tempfile()
h5createFile(f1)
h5createFile(f2)

## create two string datasets
## the first is variable length strings, the second fixed at the length of our longest word
h5createDataset(f1, "strings",
  dims = length(words), storage.mode = "character",
  size = NULL, chunk = 25
)
h5createDataset(f2, "strings",
  dims = length(words), storage.mode = "character",
  size = max(nchar(words)), chunk = 25
)

## Write the data
h5write(words, f1, "strings")
h5write(words, f2, "strings")

## Check file sizes.
## In this example the fixed length string dataset is normally much smaller
file.size(f1)
#> [1] 106865
file.size(f2)
#> [1] 59464
```
