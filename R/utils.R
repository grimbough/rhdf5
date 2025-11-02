# This helper function allows us to distinguish between:
# - an argument being omitted entirely. This is the expectation when wanting to
#   rely on the default value
# - a missing value being passed down from a higher-level function call
fix_missing_bit64conversion <- function(bit64conversion) {
  if (missing(bit64conversion)) {
    bit64conversion <- "int"
    warning(
      'To rely on the `bit64conversion` argument default, ',
      'you should omit it or explicitly set it to its default value (`"int"`).',
      ' Passing a missing value  will be disallowed in the next ',
      'release cycle.'
    )
  }
  return(bit64conversion)
}
