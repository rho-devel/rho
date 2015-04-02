library(testthat)
library(testr)

test_check("testr")
unlink("capture", recursive = T)