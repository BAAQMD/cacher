#' Declare a cached object
#'
#' @param \dots components of cache key (NOT the cached expression!)
#' @param root function that yields the path on disk
#' @param ext form of cached data (".fst" is fastest for tabular data)
#' @param verbose logical
#'
#' @seealso lazy_or
#'
#' @examples
#' cached("exp", "1", verbose = TRUE) %or% exp(1)
#'
#' @export
cached <- function (
  ...,
  root = cache_root(),
  ext = c(".rds", ".fst", ".feather", ".rds.lz4", ".rds.snz"),
  verbose = getOption("verbose")
) {
  msg <- function (...) if(isTRUE(verbose)) message("[cached] ", ...)
  key <- file.path(...)
  ext <- match.arg(ext)
  attr(key, "root") <- root
  attr(key, "ext") <- ext
  class(key) <- union(class(key), "cached")
  msg("key is: ", key)
  return(key)
}
