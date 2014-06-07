# file spatial/R/pp.R
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
#
ppinit <- function(file)
{
  tfile <- file
  t1file <- system.file("ppdata", file, package="spatial")
  if(nzchar(t1file)) tfile <- t1file
  h <- scan(tfile, list(xl = 0, xu = 0, yl = 0, yu = 0, fac = 0),
	    n = 5L, skip = 2L, quiet = TRUE)
  pp <- scan(tfile, list(x = 0, y = 0), skip = 3, quiet = TRUE)
  pp$x <- pp$x/h$fac
  pp$y <- pp$y/h$fac
  pp$area <- c(xl=h$xl/h$fac, xu=h$xu/h$fac, yl=h$yl/h$fac, yu=h$yu/h$fac)
  ppregion(pp)
  invisible(pp)
}

Kfn <- function(pp, fs, k = 100)
{
  zz <- (c(range(pp$x), range(pp$y)) - ppgetregion())*c(1,-1,1,-1)
  if(any(zz < 0)) stop("some points outside region")
  z <- .C(VR_sp_pp2,
	  as.double(pp$x),
	  as.double(pp$y),
	  as.integer(length(pp$x)),
	  k1 = as.integer(k),
	  h = double(k),
	  dmin = double(1L),
	  lm = double(1L),
	  as.double(fs))
  list(y = z$h[1L:z$k1], x = (seq(1L:z$k1) * fs)/k, k = k,
       dmin = z$dmin, lm = max(z$dmin, z$lm),
       call=match.call())
}

Kenvl <- function(fs, nsim, ...)
{
  dot.expression <- as.expression(substitute(...))
  h <- Kfn(pp = eval(dot.expression), fs)
  hx <- h$x
  hu <- h$y
  hl <- h$y
  ha <- h$y^2
  for(i in 2:nsim) {
    h <- Kfn(pp = eval(dot.expression), fs)$y
    hu <- pmax(hu, h)
    hl <- pmin(hl, h)
    ha <- ha + h^2
  }
  list(x = hx, lower = hl, upper = hu, aver = sqrt(ha/nsim),
       call=match.call())
}

Kaver <- function(fs, nsim, ...)
{
  dot.expression <- as.expression(substitute(...))
  h <- Kfn(pp = eval(dot.expression), fs)
  hx <- h$x
  ha <- h$y^2
  for(i in 2:nsim) {
    h <- Kfn(pp = eval(dot.expression), fs)$y
    ha <- ha + h^2
  }
  list(x = hx, y = sqrt(ha/nsim), call=match.call())
}

ppregion <- function(xl = 0, xu = 1, yl = 0, yu = 1)
{
    if(is.null(xl)) stop("invalid input")
    if(is.numeric(xl))
        if(length(xl) != 1L || length(xu) != 1L ||
           length(yl) != 1L || length(yu) != 1L)
            stop("invalid input")
    if(is.list(xl)) {
        if(is.null(xl$area) &&
           any(is.na(match( c("xl", "xu", "yl", "yu"), names(xl)))))
            stop("invalid input")
    }
    if(is.list(xl)) {
        if(length(xl$area)) .C(VR_ppset, as.double(xl$area))
        else .C(VR_ppset, as.double(c(xl$xl, xl$xu, xl$yl, xl$yu)))
    } else .C(VR_ppset, as.double(c(xl, xu, yl, yu)))
    invisible()
}

ppgetregion <- function()
{
    xx <- .C(VR_ppget, z=double(4))$z
    names(xx) <- c("xl", "xu", "yl", "yu")
    xx
}

Psim <- function(n)
{
  z <- .C(VR_pdata,
	  as.integer(n),
	  x = double(n),
	  y = double(n))
  invisible(list(x = z$x, y = z$y, call=match.call()))
}

Strauss <- function(n, c = 0, r)
{
  init <-  0
  if(!exists(".ppx")) {
    init <-  1
    z <- .C(VR_pdata,
	    as.integer(n),
	    x = double(n),
	    y = double(n))
    assign(".ppx", z$x)
    assign(".ppy", z$y)
  }
  z <- .C(VR_simpat,
	  as.integer(n),
	  x = as.double(.ppx),
	  y = as.double(.ppy),
	  as.double(c),
	  as.double(r),
	  as.integer(init))
  assign(".ppx", z$x)
  assign(".ppy", z$y)
  invisible(list(x = z$x, y = z$y, call=match.call()))
}

SSI <- function(n, r)
{
  z <- .C(VR_simmat,
	  as.integer(n),
	  x = double(n),
	  y = double(n),
	  as.double(r))
  invisible(list(x = z$x, y = z$y, call=match.call()))
}


pplik <- function(pp, R, ng=50, trace=FALSE)
{
    pplikfn <- function(cc, R, n, x, y, ng, target, trace=FALSE)
    {
        z <- .C(VR_plike,
                as.double(x),
                as.double(y),
                as.integer(n),
                as.double(cc),
                as.double(R),
                as.integer(ng),
                as.double(target),
                res=double(1)
                )
        if(trace) {
	    print(c(cc, z$res))
	    flush.console()
	}
        z$res
    }

  n <- length(pp$x)
  ar <- pp$area
  target <- n * (Kfn(pp, R,1)$y)^2 * pi /
    ((ar["xu"] - ar["xl"]) * (ar["yu"] - ar["yl"]))
  if(target == 0) return(0)
  tmp <- pplikfn(1, R, n, pp$x, pp$y, ng, target, FALSE)
  if(tmp <= 0) return(1)
  stats::uniroot(pplikfn, interval=c(0,1),
                 R=R, n=n, x=pp$x, y=pp$y, ng=ng, target=target,
                 trace=trace)$root
}

