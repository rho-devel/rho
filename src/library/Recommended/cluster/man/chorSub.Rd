\name{chorSub}
\alias{chorSub}
\docType{data}
\title{Subset of C-horizon of Kola Data}
\description{
  This is a small rounded subset of the C-horizon data
  \code{\link[mvoutlier]{chorizon}} from package \pkg{mvoutlier}.
}
\usage{data(chorSub)}
\format{
  A data frame with 61 observations on 10 variables.  The variables
  contain scaled concentrations of chemical elements.
}
\details{
  This data set was produced from \code{chorizon} via these statements:
  \preformatted{
    data(chorizon, package = "mvoutlier")
    chorSub <- round(100*scale(chorizon[,101:110]))[190:250,]
    storage.mode(chorSub) <- "integer"
    colnames(chorSub) <- gsub("_.*", '', colnames(chorSub))
  }
}
\source{Kola Project (1993-1998)
}
\seealso{
  \code{\link[mvoutlier]{chorizon}} in package \pkg{mvoutlier} and other
  Kola data in the same package.
}
\examples{
data(chorSub)
summary(chorSub)
pairs(chorSub, gap= .1)# some outliers
}
\keyword{datasets}
