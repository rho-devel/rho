\name{rpconvert}
\alias{rpconvert}
\title{
  Update an rpart object
}
\description{
Rpart objects changed (slightly) in their internal format in order to
accommodate the changes for user-written split functions.  This routine
updates an old object to the new format.
}
\usage{
rpconvert(x)
}
\arguments{
\item{x}{
an \code{rpart} object
}
}
\value{
An updated object
}
\seealso{
\code{rpart}
}
\keyword{tree}
