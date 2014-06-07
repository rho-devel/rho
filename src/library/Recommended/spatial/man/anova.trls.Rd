% file spatial/man/anova.trls.Rd
% copyright (C) 2001-2 W. N. Venables and B. D. Ripley
%
\name{anova.trls}
\alias{anova.trls}
\alias{anovalist.trls}
\title{
Anova tables for fitted trend surface objects
}
\description{
Compute analysis of variance tables for one or more
fitted trend surface model objects; where \code{anova.trls} is
called with multiple objects, it passes on the arguments to
\code{anovalist.trls}.
}
\usage{
\method{anova}{trls}(object, \dots)
anovalist.trls(object, \dots)
}
\arguments{
\item{object}{
A fitted trend surface model object from \code{surf.ls}
}
\item{\dots}{
Further objects of the same kind
}}
\value{
\code{anova.trls} and \code{anovalist.trls} return objects corresponding
to their printed tabular output.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{surf.ls}}
}
\examples{
library(stats)
data(topo, package="MASS")
topo0 <- surf.ls(0, topo)
topo1 <- surf.ls(1, topo)
topo2 <- surf.ls(2, topo)
topo3 <- surf.ls(3, topo)
topo4 <- surf.ls(4, topo)
anova(topo0, topo1, topo2, topo3, topo4)
summary(topo4)
}
\keyword{spatial}
