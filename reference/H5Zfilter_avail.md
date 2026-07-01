# Determine whether a filter is available on this system

Determine whether a filter is available on this system

## Usage

``` r
H5Zfilter_avail(filter_id)
```

## Arguments

- filter_id:

  Integer representing the ID of the filter to be checked.

## Examples

``` r
# bzip2 filter
H5Zfilter_avail(307)
#> [1] TRUE
```
