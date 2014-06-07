\name{summary.clara}
\alias{summary.clara}
\alias{print.summary.clara}
\title{Summary Method for `clara' Objects}
\description{
  Returns (and prints) a summary list for a \code{clara} object.
  Printing gives more output than the corresponding
  \code{\link{print.clara}} method.
}
\usage{
\method{summary}{clara}(object, \dots)
\method{print}{summary.clara}(x, \dots)
}
\arguments{
  \item{x, object}{a \code{\link{clara}} object.}
  \item{\dots}{potential further arguments (require by generic).}
}
\seealso{\code{\link{clara.object}}}
\examples{
## generate 2000 objects, divided into 5 clusters.
set.seed(47)
x <- rbind(cbind(rnorm(400, 0,4), rnorm(400, 0,4)),
           cbind(rnorm(400,10,8), rnorm(400,40,6)),
           cbind(rnorm(400,30,4), rnorm(400, 0,4)),
           cbind(rnorm(400,40,4), rnorm(400,20,2)),
           cbind(rnorm(400,50,4), rnorm(400,50,4))
)
clx5 <- clara(x, 5)
## Mis`classification' table:
% R version >= 1.5 :
% table(rep(1:5, each = 400), clx5$clust) # -> 1 "error"
table(rep(1:5, rep(400,5)), clx5$clust) # -> 1 "error"
summary(clx5)

## Graphically:
par(mfrow = c(3,1), mgp = c(1.5, 0.6, 0), mar = par("mar") - c(0,0,2,0))
%>1.5: plot(x, col = rep(2:6, each = 400))
plot(x, col = rep(2:6, rep(400,5)))
plot(clx5)
}
\keyword{cluster}
\keyword{print}
