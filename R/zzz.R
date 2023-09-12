.onLoad <- function(libname, pkgname) {
  v <- needs_setup()
  if (!v$success) {
    warning(v$msg)
  }
}
