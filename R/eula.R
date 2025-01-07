AUTO_ACCEPT_ENV_VAR <- "AUTO_ACCEPT_EULA" # nolint

#' Present EULA and Record agreement
#'
#' @return TRUE on success, FALSE on error
#'
#' @noRd
eula <- function() {
  if (eula_have_agreed()) {
    return(invisible(TRUE))
  }

  prompt_begin <- paste(
    "The LoupeR executable is subject to the 10x End User Software License,",
    "available at:\nhttps://10xgen.com/EULA \n\n"
  )

  if (auto_accepted_eula()) {
    print(paste0(
      prompt_begin,
      "You have automatically accepted the EULA by setting the `",
      AUTO_ACCEPT_ENV_VAR,
      "` environment variable"
    ))

    eula_create()
    return(invisible(TRUE))
  }

  resp <- ""
  while (!(resp %in% c("y", "yes", "n", "no"))) {
    prompt <- paste0(
      prompt_begin,
      "Do you accept the End-User License Agreement\n(y/yes or n/no): "
    )

    resp <- readline(prompt = prompt)
    resp <- tolower(resp)
  }

  if (resp %in% c("n", "no")) {
    return(FALSE)
  }

  eula_create()

  invisible(TRUE)
}

#' Create Eula lock file
#' @noRd
eula_create <- function() {
  dir.create(eula_data_dir(), showWarnings = FALSE, recursive = TRUE)
  file.create(eula_lock_file())
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

#' Check to see if we have auto accepted the eula
#' @noRd
auto_accepted_eula <- function() {
  value <- Sys.getenv(AUTO_ACCEPT_ENV_VAR, unset = "false")
  value <- trimws(tolower(value))

  return(value %in% c("t", "true", "y", "yes"))
}
