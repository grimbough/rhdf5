#include "H5PL.h"
#include "H5pubconf.h"

////////////////////////////////////////////////////
// Dynamically-loaded Plugins (H5PL)
////////////////////////////////////////////////////

/* herr_t H5PLprepend	(	const char * 	search_path	)	 */
SEXP _H5PLprepend(SEXP _search_path) {

  const char *search_path = CHAR(STRING_ELT(_search_path, 0));
  herr_t res;

  res = H5PLprepend(search_path);

  if (res < 0) {
    error("Unable to prepend value to plugin search path\n");
  }

  SEXP Rval = ScalarLogical(1);
  return Rval;
}

/* ssize_t H5PLget	(	unsigned int index, char *path_buf, size_t
 * buf_size ) */
SEXP _H5PLget(SEXP index) {

  int i = asInteger(index);
  char buf[512];
  SEXP Rval;

  if (H5PLget(i, buf, 512) < 0) {
    error("Unable to read plugin path position\n");
  }

  Rval = mkString(buf);
  return Rval;
}

/* herr_t H5PLsize	(	unsigned int * 	num_paths	) */
SEXP _H5PLsize(void) {

  SEXP Rval;
  unsigned int nvals = 0;

  if (H5PLsize(&nvals) < 0) {
    error("Unable to determine size of the plugin path\n");
  }

  if (nvals <= INT32_MAX) {
    Rval = ScalarInteger((int)nvals);
  } else {
    Rval = ScalarReal((double)nvals);
  }
  return Rval;
}
