# Include backported functions for older R versions
# This gets added to the generated NAMESPACE file
#
#' @rawNamespace
#' if (getRversion() >= "4.0.0") {
#'   importFrom(tools, R_user_dir)
#' } else {
#'   importFrom(backports, R_user_dir)
#' }
#'

# devtools::check fails with a warning if we don't reference
# at least one function from all required packages.
# The backports package namespace is never referenced directly,
# it instead overrides missings functions in other namespaces.
# Here we call the only function `backports::import` that is always
# exported, regardless of R version.
donotuse_appease_devtools_check_backports <- function() {
  backports::import()
}

.onLoad <- function(libname, pkgname) {
  v <- needs_setup()
  if (!v$success) {
    warning(v$msg)
  }
}
