cache_get <- function (..., root = cache_root(), ext = c(".rds", ".feather", ".rds.lz4", ".rds.snz"), verbose = getOption("verbose")) {

  ext <- match.arg(ext)
  pth <- cache_path(..., root = root, verbose = verbose, ext = ext)
  stopifnot(file.exists(pth))

  if (ext == ".feather") {
    require(feather)
    obj <- feather::read_feather(pth)
  } else if (ext == ".rds") {
    obj <- readRDS(pth)
  } else if (ext == ".rds.lz4") {
    con <- lz4_pipe(pth, mode = "read")
    obj <- readRDS(con)
    close(con)
  } else if (ext == ".rds.snz") {
    con <- snz_pipe(pth, mode = "read")
    obj <- readRDS(con)
    close(con)
  } else {
    stop("Unsupported extension \"", ext, "\"")
    obj <- NULL
  }

  return(obj)

}
