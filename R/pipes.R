#
# Cribbed from https://rpubs.com/jeffjjohnston/rds_compression
#
# (Thank you!!)
#

lz4_pipe <- function(filename, mode = c("read", "write"), verbose = getOption("verbose")) {

  mode <- match.arg(mode)

  con <- switch(
    mode,
    "read" = pipe(paste0("cat '", filename, "' | lz4 -d -c"), "rb"),
    "write" = pipe(paste0("lz4 -z > '", filename, "'"), "wb"))

  return(con)

}

snz_pipe <- function(filename, mode = c("read", "write"), verbose = getOption("verbose")) {

  mode <- match.arg(mode)

  con <- switch(
    mode,
    "read" = pipe(paste0("cat '", filename, "' | snzip -dc"), "rb"),
    "write" = pipe(paste0("snzip > '", filename, "'"), "wb"))

  return(con)

}
