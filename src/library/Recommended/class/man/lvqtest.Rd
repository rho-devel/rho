% file class/man/lvqtest.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{lvqtest}
\alias{lvqtest}
\title{
Classify Test Set from LVQ Codebook
}
\description{
Classify a test set by 1-NN from a specified LVQ codebook.
}
\usage{
lvqtest(codebk, test)
}
\arguments{
\item{codebk}{
codebook object returned by other LVQ software
}
\item{test}{
matrix of test examples
}}
\value{
Factor of classification for each row of \code{x}
}
\details{
Uses 1-NN to classify each test example against the codebook.
}
\references{
  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{lvqinit}}, \code{\link{olvq1}}
}
\examples{
# The function is currently defined as
function(codebk, test) knn1(codebk$x, test, codebk$cl)
}
\keyword{classif}
