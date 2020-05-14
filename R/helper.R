tmpdir <- tempfile() # yes, `tempfile()` not `tempdir()`
message("setting cache root to: ", tmpdir)

options(cacher.root = tmpdir)
