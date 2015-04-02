library(testr)
library(testthat)

context("Decoration")

test_that('Can decorate functions', {
  suppressMessages(BeginBuiltinCapture(T))
  check.dec <- sapply(builtins(T), function(x) {
    if (!exists(x, envir = getNamespace('base')))
      return(TRUE)
    obj <- get(x, envir = getNamespace('base'))
    if (is.function(obj))
      class(obj) == "functionWithTrace" || !testr:::EligibleForCapture(x) || testr:::is_s3_generic(x)
    else
      TRUE
  })
  expect_true(length(testr:::cache$decorated) > 0)
  expect_true(all(check.dec))
  suppressMessages(ClearDecoration())
  check.dec <- sapply(builtins(T), function(x) {
    if (!exists(x, envir = getNamespace('base')))
      return(FALSE)
    obj <- get(x, envir = getNamespace('base'))
    if (is.function(obj))
      class(obj) == "functionWithTrace"
    else
      FALSE
  })
  expect_true(length(testr:::cache$decorated) == 0)
  expect_false(any(check.dec))
})