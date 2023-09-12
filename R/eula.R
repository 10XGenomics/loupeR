#' Present EULA and Record agreement
#'
#' @return TRUE on success, FALSE on error
#'
#' @noRd
eula <- function() {
  if (eula_have_agreed()) {
    return(invisible(TRUE))
  }

  resp <- ""
  while(!(resp %in% c("y", "yes", "n", "no"))) {
    resp <- readline(prompt="The LoupeR executable is subject to the 10x End User Software License, available at:\nhttps://10xgen.com/EULA \n\nDo you accept the End-User License Agreement\n(y/yes or n/no): ")
    resp <- tolower(resp)
  }

  if(resp %in% c("n", "no")) {
    return(FALSE)
  }

  dir.create(eula_data_dir(), showWarnings=FALSE, recursive = TRUE)
  file.create(eula_lock_file())

  invisible(TRUE)
}

#' Reset Eula by removing lock file
#' @noRd
eula_reset <- function() {
  if (eula_have_agreed()) {
    file.remove(eula_lock_file())
  }
}

#' Check if user has agreed to EULA
#' @noRd
eula_have_agreed <- function() {
  file.exists(eula_lock_file())
}

#' Path to directory that holds EULA agreement lock file
#' @noRd
eula_data_dir <- function() {
  tools::R_user_dir("loupeR", "data")
}

#' Path to EULA agreement lock file
#' @noRd
eula_lock_file <- function() {
  file.path(eula_data_dir(), "eula_agreement")
}
