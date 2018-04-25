cache_contains <- function (..., root = cache_root()) {
  #verbose <- isTRUE(getOption("cacher.verbose"))
  file.exists(cache_path(..., root = root))
}
