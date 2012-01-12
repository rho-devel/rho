% file class/man/condense.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{condense}
\alias{condense}
\title{
Condense training set for k-NN classifier
}
\description{
Condense training set for k-NN classifier
}
\usage{
condense(train, class, store, trace = TRUE)
}
\arguments{
\item{train}{
matrix for training set
}
\item{class}{
vector of classifications for test set
}
\item{store}{
initial store set. Default one randomly chosen element of the set.
}
\item{trace}{
logical. Trace iterations?
}}
\details{
The store set is used to 1-NN classify the rest, and misclassified
patterns are added to the store set. The whole set is checked until
no additions occur.
}
\value{
Index vector of cases to be retained (the final store set).
}
\references{
P. A. Devijver and J. Kittler (1982)
\emph{Pattern Recognition. A Statistical Approach.}
Prentice-Hall, pp. 119--121.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{reduce.nn}}, \code{\link{multiedit}}
}
\examples{
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
keep <- condense(train, cl)
knn(train[keep, , drop=FALSE], test, cl[keep])
keep2 <- reduce.nn(train, keep, cl)
knn(train[keep2, , drop=FALSE], test, cl[keep2])
}
\keyword{classif}
