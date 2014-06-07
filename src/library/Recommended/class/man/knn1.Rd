% file class/man/knn1.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{knn1}
\alias{knn1}
\title{
1-nearest neighbour classification
}
\description{
Nearest neighbour classification for test set from training set. For
each row of the test set, the nearest (by Euclidean distance) training
set vector is found, and its classification used. If there is more than
one nearest, a majority vote is used with ties broken at random.
}
\usage{
knn1(train, test, cl)
}
\arguments{
\item{train}{
matrix or data frame of training set cases.
}
\item{test}{
matrix or data frame of test set cases. A vector will be interpreted
as a row vector for a single case.
}
\item{cl}{
factor of true classification of training set.
}}
\value{
Factor of classifications of test set.
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
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn1(train, test, cl)
}
\keyword{classif}
