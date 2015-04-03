library(testr)
library(testthat)

context("Generation")

test_that('Generate Abbreviate', {
  expect_error(TestGen("CaptureInfo/capture"))
  TestGen("CaptureInfo/capture_abbreviate", "abbreviate")
  expect_true(file.exists("abbreviate"))
  expect_true(file.info("abbreviate")$isdir)
  expect_equal(length(list.files("abbreviate")), 2) # one is bad.args file
  unlink("abbreviate", recursive = T)
})

test_that('Generate Abbreviate', {
  expect_error(TestGen("CaptureInfo/capture"))
  TestGen("CaptureInfo/capture_warn_error", "we")
  expect_true(file.exists("we"))
  expect_true(file.info("we")$isdir)
  expect_equal(length(list.files("we")), 3) # one is bad.args file
  expect_true(RunTests("we"))
  unlink("we", recursive = T)
})
