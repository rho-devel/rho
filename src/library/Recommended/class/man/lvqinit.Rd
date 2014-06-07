% file class/man/lvqinit.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{lvqinit}
\alias{lvqinit}
\title{
Initialize a LVQ Codebook
}
\description{
Construct an initial codebook for LVQ methods.
}
\usage{
lvqinit(x, cl, size, prior, k = 5)
}
\arguments{
\item{x}{
a matrix or data frame of training examples, \code{n} by \code{p}.
}
\item{cl}{
the classifications for the training examples. A vector or factor of
length \code{n}.
}
\item{size}{
the size of the codebook. Defaults to \code{min(round(0.4*ng*(ng-1 + p/2),0), n)}
where \code{ng} is the number of classes.
}
\item{prior}{
Probabilities to represent classes in the codebook. Default proportions in the
training set.
}
\item{k}{
k used for k-NN test of correct classification. Default is 5.
}}
\value{
A codebook, represented as a list with components \code{x} and \code{cl} giving
the examples and classes.
}
\details{
Selects \code{size} examples from the training set without replacement with
proportions proportional to the prior or the original proportions.
}
\references{
Kohonen, T. (1990) The self-organizing map.
\emph{Proc. IEEE }
\bold{78}, 1464--1480.

Kohonen, T. (1995)
\emph{Self-Organizing Maps.}
Springer, Berlin.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{lvq1}}, \code{\link{lvq2}}, \code{\link{lvq3}}, \code{\link{olvq1}}, \code{\link{lvqtest}}
}
\examples{
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
cd <- lvqinit(train, cl, 10)
lvqtest(cd, train)
cd1 <- olvq1(train, cl, cd)
lvqtest(cd1, train)
}
\keyword{classif}
