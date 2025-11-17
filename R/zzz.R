.onLoad <- function(libname, pkgname) {
  h5constants <<- H5loadConstants() # nolint: undesirable_operator_linter.
  h5errorHandling()

  if (requireNamespace("rhdf5filters")) {
    plugin_path <- rhdf5filters::hdf5_plugin_path()
    H5PLprepend(plugin_path)
  }
}
