# file MASS/R/rms.curv.R
# copyright (C) 1994-2002 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
"rms.curv"<-
function(obj)
{
  fit.val <- obj$m$fitted()
  v <- attr(fit.val, "gradient")
  if(is.null(v)) stop("\"gradient\" attribute missing")
  a <- attr(fit.val, "hessian")
  if(is.null(a)) stop("\"hessian\" attribute missing")
  p <- ncol(v)
  n <- nrow(v)
  s <- sqrt(deviance(obj)/(n - p))
  sp <- s * sqrt(p)
  D <- v
  for(j in 1L:p) D <- cbind(D, a[, 1L:j, j])
  qrd <- qr(D)
  Q <- qr.Q(qrd)
  rnk <- qrd$rank
  if(rnk <= p) warning("regression apparently linear")
  Q1 <- Q[, 1L:rnk]
  C <- array(0, c(rnk, p, p))
  for(j in 1L:p) C[,  , j] <- crossprod(Q1, a[,  , j])
  C <- aperm(C, c(2, 3, 1))
  r11i <- solve(qr.R(qrd)[1L:p, 1L:p])
  ct <- 0
  for(j in 1L:p) {
    C[,  , j] <- crossprod(r11i, C[,  , j]) %*% r11i * sp
    ct <- ct + 2 * sum(C[,  , j]^2) + sum(diag(C[,  , j]))^2
  }
  ci <- 0
  for(j in (p + 1):rnk) {
    C[,  , j] <- crossprod(r11i, C[,  , j]) %*% r11i * sp
    ci <- ci + 2 * sum(C[,  , j]^2) + sum(diag(C[,  , j]))^2
  }
  ct <- sqrt(ct/(p * (p + 2)))
  ci <- sqrt(ci/(p * (p + 2)))
  pe <- ct * sqrt(qf(19/20, p, n - p))
  ic <- ci * sqrt(qf(19/20, p, n - p))
  val <- list(pe = pe, ic = ic, ct = ct, ci = ci, C = C)
  class(val) <- "rms.curv"
  val
}
"print.rms.curv"<- function(x, ...)
{
  cat("Parameter effects: c^theta x sqrt(F) =", round(x$pe, 4), "\n",
      "       Intrinsic: c^iota  x sqrt(F) =", round(x$ic, 4), "\n",
      ...)
  invisible(x)
}
