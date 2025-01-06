#' Create a Loupe file by invoking the external Louper executable
#'
#' @param h5path path to the h5 interchange file.
#' @param output_dir optional directory where the Loupe file will be written
#' @param output_name optional name of the Loupe file with the extensions not included.
#' @param executable_path optional path to the louper executable.
#' @param force optional logical as to whether we should overwrite an already existing file
#'
#' @return TRUE on success, FALSE on error
#'
#' @noRd
louper_create_cloupe <- function(
    h5path,
    output_dir = NULL,
    output_name = NULL,
    executable_path = NULL,
    force = FALSE) {
  # default loupe name to `converted.cloupe`
  if (is.null(output_name)) {
    output_name <- "converted"
  }
  if (is.null(output_dir)) {
    loupe_path <- sprintf("%s.cloupe", file.path(output_name))
  } else {
    loupe_path <- sprintf("%s.cloupe", file.path(output_dir, output_name))
  }

  h5path <- normalizePath(path.expand(h5path))
  loupe_path <- suppressWarnings(normalizePath(path.expand(loupe_path)))

  input_flag <- sprintf("--input=%s", shQuote(h5path))
  output_flag <- sprintf("--output=%s", shQuote(loupe_path))
  args <- c("create", input_flag, output_flag)

  if (file.exists(loupe_path) && !force) {
    return(err("Loupe file already exists. Set `force = TRUE` to overwrite"))
  }

  if (force) {
    args <- c(args, "--force")
  }

  if (is.null(executable_path)) {
    executable_path <- find_executable()
    if (is.null(executable_path)) {
      return(err("Could not find a valid louper executable.  Please run `setup()`."))
    }
  }

  executable_path <- normalizePath(path.expand(executable_path))

  if (!file.exists(executable_path)) {
    return(err(sprintf("cannot find Louper executable at path: '%s'", executable_path)))
  }

  cmd_msg <- sprintf('running command: "%s"', paste(c(executable_path, args), collapse = " "))
  logMsg(cmd_msg)

  status <- system2(command = executable_path, args = args)

  if (status == 0) {
    return(SUCCESS)
  } else {
    return(err(sprintf("Louper executable failed: status code %d", status)))
  }
}
