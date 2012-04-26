% File nlme/man/balancedGrouped.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: balancedGrouped.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{balancedGrouped}
\alias{balancedGrouped}
\title{Create a groupedData object from a matrix}
\usage{
balancedGrouped(form, data, labels=NULL, units=NULL)
}
\arguments{
 \item{form}{A formula of the form \code{y ~ x | g} giving the name of
   the response, the primary covariate, and the grouping factor.}
 \item{data}{A matrix or data frame containing the values of the
   response grouped according to the levels of the grouping factor
   (rows) and the distinct levels of the primary covariate (columns).
   The \code{dimnames} of the matrix are used to construct the levels of
   the grouping factor and the primary covariate.}
 \item{labels}{an optional list of character strings giving labels for
   the response and the primary covariate.  The label for the primary
   covariate is named \code{x} and that for the response is named
   \code{y}.  Either label can be omitted.}
 \item{units}{an optional list of character strings giving the units for
   the response and the primary covariate.  The units string for the
   primary covariate is named \code{x} and that for the response is
   named \code{y}.  Either units string can be omitted.}
}
\description{
  Create a \code{groupedData} object from a data matrix.  This function
  can be used only with balanced data.  The opposite conversion, from a
  \code{groupedData} object to a \code{matrix}, is done with \code{asTable}.
}
\value{
  A balanced \code{groupedData} object.
}
\references{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
     and S-PLUS}, Springer, New York. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{groupedData}}, \code{\link{isBalanced}}, \code{\link{asTable}}}
\examples{
OrthoMat <- asTable( Orthodont )
Orth2 <- balancedGrouped(distance ~ age | Subject, data = OrthoMat,
    labels = list(x = "Age",
                  y = "Distance from pituitary to pterygomaxillary fissure"),
    units = list(x = "(yr)", y = "(mm)"))
Orth2[ 1:10, ]        ## check the first few entries

# Pinheiro and Bates, p. 109
ergoStool.mat <- asTable(ergoStool)
balancedGrouped(effort~Type|Subject,
                data=ergoStool.mat)
}
\keyword{data}
