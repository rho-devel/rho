% file class/man/reduce.nn.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{reduce.nn}
\alias{reduce.nn}
\title{
Reduce Training Set for a k-NN Classifier
}
\description{
Reduce training set for a k-NN classifier. Used after \code{condense}.
}
\usage{
reduce.nn(train, ind, class)
}
\arguments{
\item{train}{
matrix for training set
}
\item{ind}{
Initial list of members of the training set (from \code{condense}).
}
\item{class}{
vector of classifications for test set
}}
\details{
All the members of the training set are tried in random order.
Any which when dropped do not cause any members of the training set to
be wrongly classified are dropped.
}
\value{
Index vector of cases to be retained.
}
\references{
Gates, G.W. (1972) The reduced nearest neighbor rule.
\emph{IEEE Trans. Information Theory}
\bold{IT-18}, 431--432.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\eqn{\link{condense}}, \code{\link{multiedit}}
}
\examples{
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
keep <- condense(train, cl)
knn(train[keep,], test, cl[keep])
keep2 <- reduce.nn(train, keep, cl)
knn(train[keep2,], test, cl[keep2])
}
\keyword{classif}
