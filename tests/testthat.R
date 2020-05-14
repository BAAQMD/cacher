library(testthat)
library(cacher)

.v <- options(verbose = TRUE)
test_check("cacher")
options(verbose = .v)
rm(.v)
