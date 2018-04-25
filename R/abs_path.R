#' Absolute (normalized) path
#'
#' @param \dots character, as in \link{file.path}
#' @param mustWork as in \link{normalizePath}
#'
#' @return character
abs_path <- function (..., mustWork = FALSE) {
  normalizePath(file.path(...), mustWork = mustWork)
}
