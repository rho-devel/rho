% File nlme/man/asTable.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: asTable.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{asTable}
\alias{asTable}
\alias{asTable.groupedData}
\title{Convert groupedData to a matrix}
\usage{
asTable(object)
}
\arguments{
 \item{object}{A balanced \code{groupedData} object}
}
\description{
  Create a tabular representation of the response in a balanced
  groupedData object.
}
\details{
  A balanced groupedData object can be represented as a matrix or table
  of response values corresponding to the values of a primary covariate
  for each level of a grouping factor.  This function creates such a
  matrix representation of the data in \code{object}.
}
\value{
  A matrix.  The data in the matrix are the values of the response.  The
  columns correspond to the distinct values of the primary covariate and
  are labelled as such.  The rows correspond to the distinct levels of
  the grouping factor and are labelled as such.
}
\references{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{groupedData}}, \code{\link{isBalanced}},
  \code{\link{balancedGrouped}}}
\examples{
asTable(Orthodont)

# Pinheiro and Bates, p. 109
ergoStool.mat <- asTable(ergoStool)
}
\keyword{manip}
