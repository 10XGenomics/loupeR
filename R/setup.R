#' Setup eula and download executable
#'
#' @param executable_path optional string to a non default executable path
#'
#' @export
setup <- function(executable_path = NULL) {
  if (is.null(executable_path)) {
    executable_path <- find_executable()
    if (is.null(executable_path)) {
      cat("\nInstalling Executable\n\n")
      install_executable()
    }
  }

  if (!eula_have_agreed()) {
    cat("\nEULA\n\n")
    eula()
  }
}

needs_setup <- function(executable_path = NULL) {
  needs_eula <- !eula_have_agreed()
  needs_executable <- is.null(executable_path) && is.null(find_executable())

  if (needs_eula && needs_executable) {
    return(err("Please call `setup()` to install the Louper executable and to agree to the EULA before continuing"))
  }
  if (needs_eula) {
    return(err("Please call `setup()` to agree to the EULA before continuing"))
  }
  if (needs_executable) {
    return(err("Please call `setup()` to install the Louper executable"))
  }

  SUCCESS
}

#' Downloads and installs the Louper executable.
#'
#' @description
#' This is automatically called when the `louperR` library is loaded.
#'
#' @param force optional logical as to whether we should overwrite an already installed executable
#'
#' @importFrom utils download.file
#' @importFrom Seurat GetAssay
#'
#' @noRd
install_executable <- function(force = FALSE) {
  logMsg("Downloading executable")

  artifact <- get_artifact()

  destfile <- tempfile()

  # required until the package is made public and we no longer github auth
  # for now users MUST set one of these enviroment variables.
  headers <- list()
  for (envname in c('GITHUB_PAT', 'GITHUB_TOKEN')) {
    token <- Sys.getenv(x=envname)
    if (token != "") {
      headers["Authorization"] = paste0("token ", token)
      headers["Accept"] = "application/octet-stream"
      break
    }
  }

  ok <- utils::download.file(artifact$url, destfile, headers = headers, mode = "wb")
  if (ok != 0) {
    logMsg("Download failed")
    return(invisible(FALSE))
  }

  executable_path <- default_executable_path()

  if (!dir.exists(dirname(executable_path))) {
    ok <- dir.create(dirname(executable_path), showWarnings = FALSE, recursive = TRUE)
    if (!ok) {
      logMsg("Failed to create installation directory")
      return(invisible(FALSE))
    }
  }

  ok <- file.copy(destfile, executable_path, overwrite = TRUE)
  if (!ok) {
    logMsg("Failed to copy executable to final installation directory")
    return(invisible(FALSE))
  }

  ok <- Sys.chmod(executable_path, mode = "0755", use_umask = TRUE)
  if (!ok) {
    logMsg("Failed to update executable file permissions")
    return(invisible(FALSE))
  }

  v <- verify_executable(executable_path)
  if (!v$success) {
    logMsg("Verification of executable failed:", v$msg)
    return(invisible(FALSE))
  }

  invisible(v$success)
}

#' Finds the first valid executable_path
#'
#' @return string path on success, NULL if not found
#'
#' @noRd
find_executable <- function() {
  for (p in c(default_executable_path(), bundled_executable_path())) {
    res <- verify_executable(p)
    if (res$success) {
      return(p)
    }
  }

  NULL
}

#' Verify the Louper executable is installed
#'
#' @noRd
verify_executable <- function(executable_path) {
  if (!file.exists(executable_path)) {
    return(err("executable not found"))
  }

  artifact <- get_artifact()
  digest <- tools::md5sum(executable_path)
  if (digest != artifact$md5) {
    return(err("executable digest does not match"))
  }

  SUCCESS
}

#' This is executable path that will be installed via the likes of `devtools::install`
#'
#' @description
#' We use the R_user_dir instead of writing to the package install directory as the user might not have
#' permissions to write to that directory.
#'
#' @noRd
default_executable_path <- function() {
  basedir <- tools::R_user_dir("loupeR", "data")
  normalizePath(path.expand(file.path(basedir, executable_basename())), mustWork = FALSE)
}

#' This is executable path that when installing from a prebundled source tar.gz
#'
#' @description
#' Users who don't have `devtools` installed and want a simple way to install loupeR will
#' download the `tar.gz` R source package which includes in its `exec` directory the louper binary.
#'
#' @noRd
bundled_executable_path <- function() {
  basedir <- system.file("", package = "loupeR")
  normalizePath(path.expand(file.path(basedir, "exec", executable_basename())), mustWork = FALSE)
}

executable_basename <- function() {
  if (get_system_os() == "windows") {
    return("louper.exe")
  } else {
    return("louper")
  }
}

#' Louper Executable Artifacts
#' @noRd
artifacts = list(
  linux = list(
    url = "https://github.com/10XGenomics/loupeR/releases/download/v1.0.0/louper-linux-x64",
    md5 = "e3425631323e43cf9247b61a026cec09"
  ),
  mac = list(
    url = "https://github.com/10XGenomics/loupeR/releases/download/v1.0.0/louper-macos-x64",
    md5 = "fba8a41d02a00edde8671c0c44886589"
  ),
  windows = list(
    url = "https://github.com/10XGenomics/loupeR/releases/download/v1.0.0/louper-windows-x64.exe",
    md5 = "300e26db7fe5b31e196f838097afcfb9"
  )
)

#' Gets the artifact information for current OS
#' @noRd
get_artifact <- function() {
  os <- get_system_os()

  if (os == "windows") {
    return(artifacts$windows)
  } else if (os == "mac") {
    return(artifacts$mac)
  } else {
    return(artifacts$linux)
  }
}
