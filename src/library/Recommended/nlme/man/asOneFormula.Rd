% File nlme/man/asOneFormula.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: asOneFormula.Rd,v 1.5.2.1 2003/04/17 22:28:45 bates Exp $
\name{asOneFormula}
\title{Combine Formulas of a Set of Objects}
\usage{
asOneFormula(\dots, omit)
}
\alias{asOneFormula}
\arguments{
 \item{\dots}{objects, or lists of objects, from which a formula can be
   extracted.}
 \item{omit}{an optional character vector with the names of variables to
   be omitted from the returned formula. Defaults to c(".", "pi").}
}
\description{
  The names of all variables used in the formulas extracted from the
  objects defined in \code{\dots} are converted into a single linear
  formula, with the variables names separated by \code{+}. 
}
\value{
  a one-sided linear formula with all variables named in the formulas
  extracted from the objects in \code{\dots}, except the ones listed in
  \code{omit}. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{formula}}, \code{\link{all.vars}}}

\examples{
asOneFormula(y ~ x + z | g, list(~ w, ~ t * sin(2 * pi)))
}
\keyword{models}
