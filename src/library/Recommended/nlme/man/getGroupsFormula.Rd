% File nlme/man/getGroupsFormula.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroupsFormula.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{getGroupsFormula}
\title{Extract Grouping Formula}
\usage{
getGroupsFormula(object, asList, sep)
}
\alias{getGroupsFormula}
\alias{getGroupsFormula.default}
\alias{getGroupsFormula.gls}
\alias{getGroupsFormula.lmList}
\alias{getGroupsFormula.lme}
\alias{getGroupsFormula.reStruct}
\arguments{
 \item{object}{any object from which a formula can be extracted.}
 \item{asList}{an optional logical value. If \code{TRUE} the returned
   value with be a list of formulas; else, if \code{FALSE} the returned
   value will be a one-sided formula. Defaults to \code{FALSE}.}
 \item{sep}{character, the separator to use between group levels when
   multiple levels are collapsed.  The default is \code{'/'}.}
}
\description{
  The conditioning expression associated with \code{formula(object)}
  (i.e. the expression after the \code{|} operator) is returned either as
  a named list of one-sided formulas, or a single one-sided formula,
  depending on the value of \code{asList}. The components of the
  returned list are ordered from outermost to innermost level and are
  named after the grouping factor expression.
}
\value{
  a one-sided formula, or a list of one-sided formulas, with the
  grouping structure associated with \code{formula(object)}. If no
  conditioning expression is present in \code{formula(object)} a
  \code{NULL} value is returned.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
    \code{\link{getGroupsFormula.gls}},
    \code{\link{getGroupsFormula.lmList}},
    \code{\link{getGroupsFormula.lme}},
    \code{\link{getGroupsFormula.reStruct}},
    \code{\link{getGroups}}
}
\examples{
getGroupsFormula(y ~ x | g1/g2)
}
\keyword{models}
