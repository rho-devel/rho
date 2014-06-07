% file class/man/multiedit.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{multiedit}
\alias{multiedit}
\title{
Multiedit for k-NN Classifier
}
\description{
Multiedit for k-NN classifier
}
\usage{
multiedit(x, class, k = 1, V = 3, I = 5, trace = TRUE)
}
\arguments{
\item{x}{
matrix of training set.
}
\item{class}{
vector of classification of training set.
}
\item{k}{
number of neighbours used in k-NN.
}
\item{V}{
divide training set into V parts.
}
\item{I}{
number of null passes before quitting.
}
\item{trace}{
logical for statistics at each pass.
}}
\value{
Index vector of cases to be retained.
}
\references{
P. A. Devijver and J. Kittler (1982)
\emph{Pattern Recognition. A Statistical Approach.}
Prentice-Hall, p. 115.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{condense}}, \code{\link{reduce.nn}}
}
\examples{
tr <- sample(1:50, 25)
train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
cl <- factor(c(rep(1,25),rep(2,25), rep(3,25)), labels=c("s", "c", "v"))
table(cl, knn(train, test, cl, 3))
ind1 <- multiedit(train, cl, 3)
length(ind1)
table(cl, knn(train[ind1, , drop=FALSE], test, cl[ind1], 1))
ntrain <- train[ind1,]; ncl <- cl[ind1]
ind2 <- condense(ntrain, ncl)
length(ind2)
table(cl, knn(ntrain[ind2, , drop=FALSE], test, ncl[ind2], 1))
}
\keyword{classif}
