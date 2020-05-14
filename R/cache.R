#' Cache R objects on disk
#'
#' @rdname cache
#'
#' @description Store and retrieve results of arbitrary R expressions
#'
#' @param expr expression
#' @param key character (filename will be equal to `key` appended with `ext`)
#' @param compress as in [readr::read_rds()]
#' @param verbose display messages
#' @param force ignore whether an object may already be cached
#'
#' @return object (see Details)
#'
#' @details If `ext` is ".fst" or ".feather", then the returned value will be
#'   tabular data. If `ext` is ".rds", then it can be any kind of object.

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

  # Attach metadata
  metadata <- list(path = full_path, hit = (status == "hit"), force = force)
  attr(obj, "cache") <- metadata

  # Result in any case
  return(obj)

}
