% file class/man/lvq3.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{lvq3}
\alias{lvq3}
\title{
Learning Vector Quantization 3
}
\description{
Moves examples in a codebook to better represent the training set.
}
\usage{
lvq3(x, cl, codebk, niter = 100*nrow(codebk$x), alpha = 0.03,
     win = 0.3, epsilon = 0.1)
}
\arguments{
\item{x}{
a matrix or data frame of examples
}
\item{cl}{
a vector or factor of classifications for the examples
}
\item{codebk}{
a codebook
}
\item{niter}{
number of iterations
}
\item{alpha}{
constant for training
}
\item{win}{
a tolerance for the closeness of the two nearest vectors.
}
\item{epsilon}{
proportion of move for correct vectors
}}
\value{
A codebook, represented as a list with components \code{x} and \code{cl}
giving the examples and classes.
}
\details{
Selects \code{niter} examples at random  with replacement, and adjusts the nearest
two examples in the codebook for each.
}
\references{
Kohonen, T. (1990) The self-organizing map.
\emph{Proc. IEEE}
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
  \code{\link{lvqinit}}, \code{\link{lvq1}}, \code{\link{olvq1}},
  \code{\link{lvq2}}, \code{\link{lvqtest}}
}
\examples{
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
cd <- lvqinit(train, cl, 10)
lvqtest(cd, train)
cd0 <- olvq1(train, cl, cd)
lvqtest(cd0, train)
cd3 <- lvq3(train, cl, cd0)
lvqtest(cd3, train)
}
\keyword{classif}
