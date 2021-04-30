# Process this file with autoconf to produce a configure script.
# author: Mike Smith (grimbough@gmail.com)

AC_PREREQ([2.65])
AC_INIT([rhdf5], [2.41], [grimbough@gmail.com])
AC_CONFIG_SRCDIR([src])

# determine if the filters package is installed
FILTERS=`"${R_HOME}/bin${R_ARCH_BIN}/Rscript" -e 'cat(if(require("rhdf5filters", quietly = TRUE)) "_H5P_filters" else "undef")' | \
    tail -n1`

AC_SUBST(FILTERS)
AC_CONFIG_FILES([src/external_filters.h])
AC_OUTPUT
