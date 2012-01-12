% file spatial/man/trls.influence.Rd
% copyright (C) 2001-2 W. N. Venables and B. D. Ripley
%
\name{trls.influence}
\alias{trls.influence}
\alias{plot.trls}
\title{
  Regression diagnostics for trend surfaces
}
\description{
  This function provides the basic quantities which are used in
  forming a variety of diagnostics for checking the quality of
  regression fits for trend surfaces calculated by \code{surf.ls}.
}
\usage{
trls.influence(object)
\method{plot}{trls}(x, border = "red", col = NA, pch = 4, cex = 0.6,
     add = FALSE, div = 8, \dots)
}
\arguments{
  \item{object, x}{
    Fitted trend surface model from \code{surf.ls}
  }
  \item{div}{
    scaling factor for influence circle radii in \code{plot.trls}
  }
  \item{add}{
    add influence plot to existing graphics if \code{TRUE}
  }
  \item{border, col, pch, cex, \dots}{
    additional graphical parameters
  }
}
\value{
  \code{trls.influence} returns a list with components:
  
  \item{r}{
    raw residuals as given by \code{residuals.trls}
  }
  \item{hii}{
    diagonal elements of the Hat matrix
  }
  \item{stresid}{
    standardised residuals
  }
  \item{Di}{
    Cook's statistic
  }
}
\references{
  Unwin, D. J., Wrigley, N. (1987) Towards a general-theory of
  control point distribution effects in trend surface models.
  \emph{Computers and Geosciences,} \bold{13}, 351--355.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{surf.ls}}, \code{\link{influence.measures}}, \code{\link{plot.lm}}
}
\examples{
library(MASS)  # for eqscplot
data(topo, package = "MASS")
topo2 <- surf.ls(2, topo)
infl.topo2 <- trls.influence(topo2)
(cand <- as.data.frame(infl.topo2)[abs(infl.topo2$stresid) > 1.5, ])
cand.xy <- topo[as.integer(rownames(cand)), c("x", "y")]
trsurf <- trmat(topo2, 0, 6.5, 0, 6.5, 50)
eqscplot(trsurf, type = "n")
contour(trsurf, add = TRUE, col = "grey")
plot(topo2, add = TRUE, div = 3)
points(cand.xy, pch = 16, col = "orange")
text(cand.xy, labels = rownames(cand.xy), pos = 4, offset = 0.5)
}
\keyword{spatial}
