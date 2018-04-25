#' @export
cache_root <- function (..., root = NULL, verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[cache_root] ", ...)

  # First fallback
  if (is.null(root)) {
    root <- getOption("cacher.root")
  }

  # Second fallback
  if (is.null(root)) {
    root <- normalizePath(file.path("~", "cacher"), mustWork = FALSE)
    options(cacher.root = root)
  }

  if (!dir.exists(root)) {
    msg("creating: ", root)
    dir.create(root, recursive = TRUE)
  }

  dn <- file.path(root, ...)
  if (!file.exists(dn)) dir.create(dn, recursive = TRUE)

  msg("root is: ", dn)
  return(dn)

}
