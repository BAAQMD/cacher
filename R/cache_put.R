cache_put <- function (obj, ..., root = cache_root(), ext = c(".rds", ".feather", ".rds.lz4", ".rds.snz"), verbose = getOption("verbose")) {

  ext <- match.arg(ext)
  pth <- cache_path(..., root = root, verbose = verbose, ext = ext)

  obj <- force(obj)
  if (is.null(obj)) return(obj) # don't bother caching NULL!

  if (verbose) flog("Caching as: ", pth, level = "debug")

  if (ext == ".feather") {
    require(feather) # experimental support for https://github.com/wesm/feather
    stopifnot(is.data.frame(obj))
    feather::write_feather(obj, path = pth)
  } else if (ext == ".rds") {
    saveRDS(obj, file = pth)
  } else if (ext == ".rds.lz4") {
    con <- lz4_pipe(pth, mode = "write")
    saveRDS(obj, file = con)
    flush(con)
    close(con)
  } else if (ext == ".rds.snz") {
    con <- snz_pipe(pth, mode = "write")
    saveRDS(obj, file = con)
    flush(con)
    close(con)
  } else {
    stop("Unsupported extension \"", ext, "\"")
  }

  return(obj)

}
