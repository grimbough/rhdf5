# This helper function allows us to distinguish between:
# - an argument being omitted entirely. This is the expectation when wanting to
#   rely on the default value
# - a missing value being passed down from a higher-level function call
fix_missing <- function(x, default) {
  if (missing(x)) {
    warning(
      sprintf(
        'To rely on the `%s` argument default, you should omit omit it or explicitly set it to its default value (`"%s"`).',
        deparse(substitute(x)),
        default
      ),
      ' Passing a missing value  will be disallowed in the next release cycle.',
      call. = FALSE
    )
    x <- "int"
  }
  return(x)
}
