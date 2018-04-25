#' Cache R objects on disk
#'
#' @description
#' Store and retrieve results of arbitrary R expressions in .Rds files
#'
#' @param key character (filename will be \code{key + .rds})
#' @param expr expression
#' @param path character
#' @param compress as in \link{readRDS}
#' @param verbose logical
#' @param force logical
#' @param \dots further arguments
#'
#' @note Watch out for case-insensitive filesystems!
#'
#' @name cache
#' @export
cache <- function (
  expr,
  key,
  verbose = getOption("verbose"),
  force = FALSE
) {

  # Standardize path
  full_path <- cache_path(key)

  # Core logic
  if (force) {
    status <- "force"
    obj <- cache_put(full_path, expr)
  } else if (!file.exists(full_path)) {
    status <- "miss"
    obj <- cache_put(full_path, expr)
  } else {
    status <- "hit"
    obj <- cache_get(full_path)
  }

  # For debugging
  if (verbose) flog("[cache] ", status, ": ", full_path, level = "debug")

  # Attach metadata
  metadata <- list(path = full_path, hit = (status == "hit"), force = force)
  attr(obj, "cache") <- metadata

  # Result in any case
  return(obj)

}
