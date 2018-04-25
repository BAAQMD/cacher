#' @export
`%or%` <- function (x, y, ...) UseMethod("%or%")

#' @export
`%or%.default` <- function (x, y) {
  if (is.null(x)) {
    return(y)
  } else {
    return(x)
  }
}

#' @export
`%or%.cached` <- function (x, y) {
  root <- attr(x, "root")
  ext = attr(x, "ext")
  key <- x
  verbose <- isTRUE(getOption("cacher.verbose")) || isTRUE(options("verbose"))
  msg <- function (...) if(isTRUE(verbose)) message("[%or%.cached] ", ...)
  obj <- NULL; status <- "FAIL"
  if (cache_contains(key, root = root, ext = ext)) {
    status <- "HIT"
    tryCatch(
      obj <- cache_get(key, root = root, ext = ext),
      error = function (e) flog("error reading from ", key, level = "warn"))
  } else {
    status <- "MISS"
    obj <- cache_put(y, key, root = root, ext = ext)
  }
  msg(key, " was a ", status)
  flog(msg, status, level = "debug")
  return(obj)
}
