% file class/man/knn.cv.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{knn.cv}
\alias{knn.cv}
\title{
k-Nearest Neighbour Cross-Validatory Classification
}
\description{
k-nearest neighbour cross-validatory classification from training set.
}
\usage{
knn.cv(train, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
}
\arguments{
\item{train}{
matrix or data frame of training set cases.
}
\item{cl}{
factor of true classifications of training set
}
\item{k}{
number of neighbours considered.
}
\item{l}{
minimum vote for definite decision, otherwise \code{doubt}. (More
precisely, less than \code{k-l} dissenting votes are allowed, even
if \code{k} is increased by ties.)
}
\item{prob}{
If this is true, the proportion of the votes for the winning class
are returned as attribute \code{prob}.
}
\item{use.all}{
controls handling of ties. If true, all distances equal to the \code{k}th
largest are included. If false, a random selection of distances
equal to the \code{k}th is chosen to use exactly \code{k} neighbours.
}}
\details{
This uses leave-one-out cross validation.
For each row of the training set \code{train}, the \code{k} nearest
(in Euclidean distance) other
training set vectors are found, and the classification is decided by
majority vote, with ties broken at random. If there are ties for the
\code{k}th nearest vector, all candidates are included in the vote.
}
\value{
Factor of classifications of training set. \code{doubt} will be returned as \code{NA}.
}
\references{
  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{knn}}
}
\examples{
train <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
cl <- factor(c(rep("s",50), rep("c",50), rep("v",50)))
knn.cv(train, cl, k = 3, prob = TRUE)
attributes(.Last.value)
}
\keyword{classif}
