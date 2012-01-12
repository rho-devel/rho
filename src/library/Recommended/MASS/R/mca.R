# file MASS/R/mca.R
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
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
mca <- function(df, nf = 2, abbrev = FALSE)
{
  class.ind <- function(cl)
  {
    n <- length(cl); cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
  }
  if(!all(unlist(lapply(df, is.factor))))
    stop("all variables must be factors")
  Call <- match.call()
  n <- nrow(df); p <- length(df)
  G <- as.matrix(do.call("data.frame", c(lapply(df, class.ind),
                                         check.names=FALSE)))
  Dc <- drop((rep(1, n)) %*% G)
  X <- t(t(G)/(sqrt(p*Dc)))
  X.svd <- svd(X)
  sec <- 1 + (1L:nf)
  rs <- X %*% X.svd$v[, sec]/p
  cs <- diag(1/(sqrt(p*Dc))) %*% X.svd$v[, sec]
  fs <- X.svd$u[, sec]/rep(p*X.svd$d[sec], rep(n, nf))
  dimnames(rs) <- list(row.names(df), as.character(1L:nf))
  dimnames(fs) <- dimnames(rs)
  varnames <- if(abbrev) unlist(lapply(df, levels))
              else colnames(G)
  dimnames(cs) <- list(varnames, as.character(1L:nf))
  structure(list(rs=rs, cs=cs, fs=fs, d=X.svd$d[sec], p=p, call=Call),
            class="mca")
}

print.mca <- function(x, ...)
{
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl, control=NULL)
  }
  cat("\nMultiple correspondence analysis of",
            nrow(x$rs), "cases of", x$p,
            "factors\n")
  cat("\nCorrelations", format(round(x$d,3), ...))
  p <- 100 * cumsum(x$d)/(x$p - 1)
  cat("  cumulative % explained", format(round(p,2), ...), "\n")
  invisible(x)
}

plot.mca <- function(x, rows = TRUE,
                     col, cex = par("cex"), ...)
{
  if(length(cex) == 1L) cex <- rep(cex, 2L)
  eqscplot(x$cs, type="n", xlab="", ...)
  if(missing(col)) {
    col <- par("col")
    if (!is.numeric(col)) col <- match(col, palette())
    col <- c(col, col + 1L)
  } else if(length(col) != 2L) col <- rep(col, length = 2L)
  if(rows) text(x$rs, cex=cex[1L], col=col[1L])
  text(x$cs, labels=dimnames(x$cs)[[1L]], cex=cex[2L], col=col[2L])
  invisible(x)
}

predict.mca <- function(object, newdata, type=c("row", "factor"), ...)
{
  class.ind <- function(cl)
  {
    n <- length(cl); cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
  }

  type <- match.arg(type)
  if(is.null(abbrev <- object$call$abbrev)) abbrev <- FALSE
  if(!all(unlist(lapply(newdata, is.factor))))
    stop("all variables must be factors")
  G <- as.matrix(do.call("data.frame", c(lapply(newdata, class.ind),
                                         check.names=FALSE)))
  if(abbrev) colnames(G) <- unlist(lapply(newdata, levels))
  if(type == "row") {
    # predict new row(s)
    if(!all(colnames(G) == dimnames(object$cs)[[1L]]))
       stop("factors in 'newdata' do not match those for 'object'")
    G %*% object$cs/object$p
  } else {
    # predict positions of new factor(s)
    n <- nrow(G)
    Dc <- drop((rep(1, n)) %*% G)
    if(n != nrow(object$fs))
      stop("'newdata' is not of the right length")
    (t(G)/Dc) %*% object$fs
  }
}
