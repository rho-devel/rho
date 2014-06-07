\name{twins.object}
\alias{twins.object}
\alias{twins}% == class
\title{Hierarchical Clustering Object}

\description{
  The objects of class \code{"twins"} represent an agglomerative or
  divisive (polythetic) hierarchical clustering of a dataset.
}
\section{GENERATION}{
  This class of objects is returned from \code{agnes} or \code{diana}.
}
\section{METHODS}{
  The \code{"twins"} class has a method for the following generic function:
  \code{pltree}.
}
\section{INHERITANCE}{
  The following classes inherit from class \code{"twins"} :
  \code{"agnes"} and \code{"diana"}.
}
\value{
  See \code{\link{agnes.object}} and \code{\link{diana.object}} for details.
}
\seealso{\code{\link{agnes}},\code{\link{diana}}.
}
\keyword{cluster}
