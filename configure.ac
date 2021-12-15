# Process this file with autoconf to produce a configure script.
# author: Mike Smith (grimbough@gmail.com)

AC_PREREQ([2.65])
AC_INIT([rhdf5], [2.41], [grimbough@gmail.com])
AC_CONFIG_SRCDIR([src])

# determine if the filters package is installed
FILTERS=`"${R_HOME}/bin${R_ARCH_BIN}/Rscript" -e 'quit(status = !require("rhdf5filters", quietly = TRUE))' 1> /dev/null && echo _H5P_filters || echo undef`

AC_SUBST(FILTERS)
AC_CONFIG_FILES([src/external_filters.h])
AC_OUTPUT
