# Process this file with autoconf to produce a configure script.
# author: Mike Smith (grimbough@gmail.com)

AC_PREREQ([2.65])
AC_INIT([rhdf5], [2.41], [grimbough@gmail.com])
AC_CONFIG_SRCDIR([src])

# determine if the filters package is installed
FILTERS=`echo 'cat(if("rhdf5filters" %in% installed.packages()[[,"Package"]]) "_H5P_filters" else "undef")'|\
    "${R_HOME}/bin/R" --vanilla --slave`

AC_SUBST(FILTERS)
AC_CONFIG_FILES([src/external_filters.h])
AC_OUTPUT
