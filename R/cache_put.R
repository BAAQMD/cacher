#' Put content in the cache
#'
#' @rdname cache
#'
#' @param obj object
#'
#' @export
cache_put <- function (
  obj,
  ...,
  root = cache_root(),
  ext = c(".rds", ".fst", ".feather", ".rds.lz4", ".rds.snz"),
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[cache_put] ", ...)

  ext <- match.arg(ext)
  pth <- cache_path(..., root = root, verbose = verbose, ext = ext)

  obj <- force(obj)
  if (is.null(obj)) return(obj) # don't bother caching NULL!

  msg("writing to: ", pth)

  if (ext == ".rds") {

    readr::write_rds(obj, pth)

  } else if (ext == ".fst") {

    msg("writing tabular data to ", pth)
    stopifnot(is.data.frame(obj))
    tbl <- force(tibble::as_tibble(obj))

    fst::write_fst(obj, pth)

  } else if (ext == ".feather") {

    msg("writing tabular data to ", pth)
    stopifnot(is.data.frame(obj))
    tbl <- force(tibble::as_tibble(obj))

    feather::write_feather(tbl, pth)

  } else if (ext == ".rds.lz4") {

    con <- lz4_pipe(pth, mode = "write")
    readr::write_rds(obj, file = con)
    flush(con)
    close(con)

  } else if (ext == ".rds.snz") {

    con <- snz_pipe(pth, mode = "write")
    readr::write_rds(obj, file = con)
    flush(con)
    close(con)

  } else {

    stop("Unsupported extension \"", ext, "\"")

  }

  return(obj)

}
