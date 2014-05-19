\name{uniquecombs}
\alias{uniquecombs}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{find the unique rows in a matrix }
\description{
This routine returns a matrix containing all the unique rows of the
matrix supplied as its argument. That is, all the duplicate rows are
stripped out. Note that the ordering of the rows on exit is not the same
as on entry. It also returns an index attribute for relating the result back 
to the original matrix.
}
\usage{
uniquecombs(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{x}{ is an \R matrix (numeric) }
}
\details{ Models with more parameters than unique combinations of
  covariates are not identifiable. This routine provides a means of
  evaluating the number of unique combinations of coavariates in a
  model. The routine calls compiled C code.
   
}
\value{
A matrix consisting of the unique rows of \code{x} (in arbitrary order).

The matrix has an \code{"index"} attribute. \code{index[i]} gives the row of the returned 
matrix that contains row i of the original matrix. 
 
}

\seealso{\code{\link{unique}} can do the same thing, including for non-numeric matrices, but more 
         slowly and without returning the index.}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\examples{
require(mgcv)
X<-matrix(c(1,2,3,1,2,3,4,5,6,1,3,2,4,5,6,1,1,1),6,3,byrow=TRUE)
print(X)
Xu <- uniquecombs(X);Xu
ind <- attr(Xu,"index")
## find the value for row 3 of the original from Xu
Xu[ind[3],];X[3,]
}
\keyword{models} \keyword{regression}%-- one or more ..


