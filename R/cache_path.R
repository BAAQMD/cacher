cache_path <- function (..., root = cache_root(), verbose = isTRUE(getOption("cacher.verbose")), ext = ".rds") {
  path <- normalizePath(paste0(file.path(root, ...), ext), mustWork = FALSE)
  if (!file.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  return(path)
}
