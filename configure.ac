# Process this file with autoconf to produce a configure script.
# author: Mike Smith (grimbough@gmail.com)

AC_PREREQ([2.65])
AC_INIT([hdf5Filters], [0.0.1], [grimbough@gmail.com])
AC_CONFIG_SRCDIR([src])

# determine if the filters package is installed
FILTERS=`echo 'cat(if("hdf5Filters" %in% installed.packages()[[,"Package"]]) "_H5P_filters" else "undef")'|\
    "${R_HOME}/bin/R" --vanilla --slave`
    
echo $FILTERS

INCLUDE_PATH=`echo 'cat(if("hdf5Filters" %in% installed.packages()[[,"Package"]]) hdf5Filters::pkgconfig("PKG_LIB") else "")'|\
    "${R_HOME}/bin/R" --vanilla --slave`
    
echo $INCLUDE_PATH

AC_SUBST(FILTERS)
AC_CONFIG_FILES([src/external_filters.h])
AC_OUTPUT

AC_SUBST(INCLUDE_PATH)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
