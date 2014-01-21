\name{is.null.DN}
\alias{is.null.DN}
\title{Are the Dimnames \code{dn} NULL-like ?}
\description{
  Are the \code{\link{dimnames}} \code{dn} \code{\link{NULL}}-like?

  \code{is.null.DN(dn)} is less strict than \code{\link{is.null}(dn)},
  because it is also true (\code{\link{TRUE}}) when the dimnames
  \code{dn} are \dQuote{like} \code{NULL}, or \code{list(NULL,NULL)}, as
  they can easily be for the traditional \R matrices
  (\code{\link{matrix}}) which have no formal \code{\link{class}}
  definition, and hence much freedom in how their \code{\link{dimnames}}
  look like.
}
\usage{
  is.null.DN(dn)
}
\arguments{
  \item{dn}{\code{\link{dimnames}()} of a \code{\link{matrix}}-like \R
    object.
  }
}
\note{
  This function is really to be used on \dQuote{traditional} matrices
  rather than those inheriting from \code{\linkS4class{Matrix}}, as
  the latter will always have dimnames \code{list(NULL,NULL)} exactly,
  in such a case.
}
\value{
  \code{\link{logical}} \code{\link{TRUE}} or \code{\link{FALSE}}.
}
%% \details{
%% }
\author{Martin Maechler}
\seealso{
  \code{\link{is.null}},
  \code{\link{dimnames}}, \code{\link{matrix}}.
}
\examples{
m <- matrix(round(100 * rnorm(6)), 2,3); m1 <- m2 <- m3 <- m4 <- m
dimnames(m1) <- list(NULL, NULL)
dimnames(m2) <- list(NULL, character())
dimnames(m3) <- rev(dimnames(m2))
dimnames(m4) <- rep(list(character()),2)

m4 ## prints absolutely identically to  m

stopifnot(m == m1, m1 == m2, m2 == m3, m3 == m4,
	  identical(capture.output(m) -> cm,
		    capture.output(m1)),
	  identical(cm, capture.output(m2)),
	  identical(cm, capture.output(m3)),
	  identical(cm, capture.output(m4)))
}
\keyword{utilities}
