library(testr)
library(testthat)

context("Corner cases")

test_that('Missing argument corner case is fixed', {
  # was failing before 5fe109579d670a0ebf094215398410ca08c3749b
  suppressMessages(Decorate(qr.X))
  p <- ncol(x <- LifeCycleSavings[,-1]) # not the `sr'
  qrstr <- qr(x)   # dim(x) == c(n,p)
  X <- qr.X(qrstr) # X == x
  dim(Xc <- qr.X(qrstr, complete=TRUE)) # square: nrow(x) ^ 2
  dimnames(X) <- NULL
  expect_true(all.equal(Xc[,1:p], X))

  # was failing before 5fe109579d670a0ebf094215398410ca08c3749b
  suppressMessages(Decorate(svd))
  Meps <- .Machine$double.eps
  Eps <- 100 * Meps
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  X <- hilbert(9)[,1:6]
  s <- svd(X)
  D <- diag(s$d)
  expect_true(all(abs(X - s$u %*% D %*% t(s$v)) < Eps))#  X = U D V'
  expect_true(all(abs(D - t(s$u) %*% X %*% s$v) < Eps))#  D = U' X V  
})

test_that('Arguments are evaluated in the proper environment', {
  # was failing before evaluation in wrong environment
#   foo <- function(what){
#     what
#   }
#   Decorate(foo)
#   bar <- function() foo(sys.nframe())
#   bar()
#   expect_equal(bar.res, sys.nframe() + 1)
})