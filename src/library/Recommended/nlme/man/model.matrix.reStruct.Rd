% File nlme/man/model.matrix.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: model.matrix.reStruct.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{model.matrix.reStruct}
\title{reStruct Model Matrix}
\usage{
\method{model.matrix}{reStruct}(object, data, contrast, \dots)
}
\alias{model.matrix.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{data}{a data frame in which to evaluate the variables defined in
   \code{formula(object)}.}
 \item{contrast}{an optional named list specifying the contrasts to be used
   for representing the \code{factor} variables in \code{data}. The
   components names should match the names of the variables in
   \code{data} for which the contrasts are to be specified. The
   components of this list will be used as the \code{contrasts}
   attribute of the corresponding factor. If missing, the default
   contrast specification is used.}  
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The model matrices for each element of \code{formula(object)},
  calculated using \code{data}, are bound together column-wise. When
  multiple grouping levels are present (i.e. when \code{length(object) >
    1}), the individual model matrices are combined from innermost (at
  the leftmost position) to outermost (at the rightmost position). 
}
\value{
  a matrix obtained by binding together, column-wise, the model matrices
  for each element of \code{formula(object)}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{model.matrix}}, \code{\link{contrasts}},
  \code{\link{reStruct}}, \code{\link{formula.reStruct}}} 

\examples{
rs1 <- reStruct(list(Dog = ~day, Side = ~1), data = Pixel)
model.matrix(rs1, Pixel)
}
\keyword{models}
