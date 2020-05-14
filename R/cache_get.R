#' Get content from the cache
#'
#' @rdname cache
#'
#' @export
cache_get <- function (
  ...,
  root = cache_root(),
  ext = c(".rds", ".fst", ".feather", ".rds.lz4", ".rds.snz"),
  verbose = getOption("verbose")
) {

  ext <- match.arg(ext)
  pth <- cache_path(..., root = root, verbose = verbose, ext = ext)
  stopifnot(file.exists(pth))

  if (ext == ".rds") {
    obj <- readr::read_rds(pth)
  } else if (ext == ".fst") {
    obj <- tibble::as_tibble(fst::read_fst(pth))
  } else if (ext == ".feather") {
    obj <- tibble::as_tibble(feather::read_feather(pth))
  } else if (ext == ".rds.lz4") {
    con <- lz4_pipe(pth, mode = "read")
    obj <- readr::read_rds(con)
    close(con)
  } else if (ext == ".rds.snz") {
    con <- snz_pipe(pth, mode = "read")
    obj <- readr::read_rds(con)
    close(con)
  } else {
    stop("Unsupported extension \"", ext, "\"")
    obj <- NULL
  }

  return(obj)

}
