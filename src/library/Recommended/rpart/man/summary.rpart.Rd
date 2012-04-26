\name{summary.rpart}
\alias{summary.rpart}
\title{
  Summarize a Fitted Rpart Object 
}
\description{
  Returns a detailed listing of a fitted \code{rpart} object.
}
\usage{
\method{summary}{rpart}(object, cp=0, digits=getOption("digits"), file, \dots)
}
\arguments{
\item{object}{
  fitted model object of class \code{rpart}.  This is assumed to be the result
  of some function that produces an object with the same named
  components as that returned by the \code{rpart} function.
}
\item{digits}{
    Number of significant digits to be used in the result.
}
\item{cp}{
  trim nodes with a complexity of less than \code{cp} from the listing.
}
\item{file}{
  write the output to a given file name. (Full listings of a tree are
  often quite long).
}
\item{\dots}{
  arguments to be passed to or from other methods.
}}
\details{
  This function is a method for the generic function summary for class
  \code{"rpart"}.  It can be invoked by calling \code{summary}
  for an object of the appropriate class, or directly by calling
  \code{summary.rpart} regardless of the class of the object.
}
\seealso{
  \code{\link{summary}}, \code{\link{rpart.object}}, \code{\link{printcp}}.
}
\examples{
z.auto <- rpart(Mileage ~ Weight, car.test.frame)
summary(z.auto)
}
\keyword{tree}
