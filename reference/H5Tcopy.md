# Copy an existing datatype

Copy an existing datatype

## Usage

``` r
H5Tcopy(dtype_id = h5default(type = "H5T"))
```

## Arguments

- dtype_id:

  Datatype to copy. Can either be a character specifying a predefined
  HDF5 datatype (see `h5const("H5T")` for valid options) or the ID of an
  already created datatype.

## Examples

``` r
h5const("H5T")
#>  [1] "H5T_IEEE_F32BE"          "H5T_IEEE_F32LE"         
#>  [3] "H5T_IEEE_F64BE"          "H5T_IEEE_F64LE"         
#>  [5] "H5T_STD_I8BE"            "H5T_STD_I8LE"           
#>  [7] "H5T_STD_I16BE"           "H5T_STD_I16LE"          
#>  [9] "H5T_STD_I32BE"           "H5T_STD_I32LE"          
#> [11] "H5T_STD_I64BE"           "H5T_STD_I64LE"          
#> [13] "H5T_STD_U8BE"            "H5T_STD_U8LE"           
#> [15] "H5T_STD_U16BE"           "H5T_STD_U16LE"          
#> [17] "H5T_STD_U32BE"           "H5T_STD_U32LE"          
#> [19] "H5T_STD_U64BE"           "H5T_STD_U64LE"          
#> [21] "H5T_STD_B8BE"            "H5T_STD_B8LE"           
#> [23] "H5T_STD_B16BE"           "H5T_STD_B16LE"          
#> [25] "H5T_STD_B32BE"           "H5T_STD_B32LE"          
#> [27] "H5T_STD_B64BE"           "H5T_STD_B64LE"          
#> [29] "H5T_NATIVE_CHAR"         "H5T_NATIVE_SCHAR"       
#> [31] "H5T_NATIVE_UCHAR"        "H5T_NATIVE_SHORT"       
#> [33] "H5T_NATIVE_USHORT"       "H5T_NATIVE_INT"         
#> [35] "H5T_NATIVE_UINT"         "H5T_NATIVE_LONG"        
#> [37] "H5T_NATIVE_ULONG"        "H5T_NATIVE_LLONG"       
#> [39] "H5T_NATIVE_ULLONG"       "H5T_NATIVE_FLOAT"       
#> [41] "H5T_NATIVE_DOUBLE"       "H5T_NATIVE_LDOUBLE"     
#> [43] "H5T_NATIVE_B8"           "H5T_NATIVE_B16"         
#> [45] "H5T_NATIVE_B32"          "H5T_NATIVE_B64"         
#> [47] "H5T_NATIVE_OPAQUE"       "H5T_NATIVE_HADDR"       
#> [49] "H5T_NATIVE_HSIZE"        "H5T_NATIVE_HSSIZE"      
#> [51] "H5T_NATIVE_HERR"         "H5T_NATIVE_HBOOL"       
#> [53] "H5T_NATIVE_INT8"         "H5T_NATIVE_UINT8"       
#> [55] "H5T_NATIVE_INT_LEAST8"   "H5T_NATIVE_UINT_LEAST8" 
#> [57] "H5T_NATIVE_INT_FAST8"    "H5T_NATIVE_UINT_FAST8"  
#> [59] "H5T_NATIVE_INT16"        "H5T_NATIVE_UINT16"      
#> [61] "H5T_NATIVE_INT_LEAST16"  "H5T_NATIVE_UINT_LEAST16"
#> [63] "H5T_NATIVE_INT_FAST16"   "H5T_NATIVE_UINT_FAST16" 
#> [65] "H5T_NATIVE_INT32"        "H5T_NATIVE_UINT32"      
#> [67] "H5T_NATIVE_INT_LEAST32"  "H5T_NATIVE_UINT_LEAST32"
#> [69] "H5T_NATIVE_INT_FAST32"   "H5T_NATIVE_UINT_FAST32" 
#> [71] "H5T_NATIVE_INT64"        "H5T_NATIVE_UINT64"      
#> [73] "H5T_NATIVE_INT_LEAST64"  "H5T_NATIVE_UINT_LEAST64"
#> [75] "H5T_NATIVE_INT_FAST64"   "H5T_NATIVE_UINT_FAST64" 
#> [77] "H5T_NATIVE_DOUBLE"       "H5T_C_S1"               
#> [79] "H5T_FORTRAN_S1"          "H5T_STD_REF_OBJ"        
#> [81] "H5T_STD_REF_DSETREG"    
tid <- H5Tcopy("H5T_C_S1")
```
