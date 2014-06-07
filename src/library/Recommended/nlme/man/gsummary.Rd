% File nlme/man/gsummary.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: gsummary.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{gsummary}
\title{Summarize by Groups}
\usage{
gsummary(object, FUN, omitGroupingFactor, form, level,
   groups, invariantsOnly, \dots)
}
\alias{gsummary}
\arguments{
 \item{object}{an object to be summarized - usually a \code{groupedData}
   object or a \code{data.frame}.
 }
 \item{FUN}{an optional summary function or a list of summary functions
   to be applied to each variable in the frame.  The function or
   functions are applied only to variables in \code{object} that vary
   within the groups defined by \code{groups}.  Invariant variables are
   always summarized by group using the unique value that they assume
   within that group.  If \code{FUN} is a single
   function it will be applied to each non-invariant variable by group
   to produce the summary for that variable.  If \code{FUN} is a list of
   functions, the names in the list should designate classes of
   variables in the frame such as \code{ordered}, \code{factor}, or
   \code{numeric}.  The indicated function will be applied to any
   non-invariant variables of that class.  The default functions to be
   used are \code{mean} for numeric factors, and \code{Mode} for both
   \code{factor} and \code{ordered}.  The \code{Mode} function, defined
   internally in \code{gsummary}, returns the modal or most popular
   value of the variable.  It is different from the \code{mode} function
   that returns the S-language mode of the variable.
 }
 \item{omitGroupingFactor}{an optional logical value.  When \code{TRUE}
   the grouping factor itself will be omitted from the group-wise
   summary but the levels of the grouping factor will continue to be
   used as the row names for the data frame that is produced by the
   summary. Defaults to \code{FALSE}.
 }
 \item{form}{an optional one-sided formula that defines the groups.
   When this formula is given, the right-hand side is evaluated in
   \code{object}, converted to a factor if necessary, and the unique
   levels are used to define the groups.  Defaults to
   \code{formula(object)}.
 } 
 \item{level}{an optional positive integer giving the level of grouping
   to be used in an object with multiple nested grouping levels.
   Defaults to the highest or innermost level of grouping.}
 \item{groups}{an optional factor that will be used to split the 
   rows into groups.  Defaults to \code{getGroups(object, form, level)}.
 }
 \item{invariantsOnly}{an optional logical value.  When \code{TRUE} only 
   those covariates that are invariant within each group will be
   summarized.  The summary value for the group is always the unique
   value taken on by that covariate within the group.  The columns in
   the summary are of the same class as the corresponding columns in
   \code{object}. By definition, the grouping factor itself must be an
   invariant.   When combined with \code{omitGroupingFactor = TRUE},
   this option can be used to discover is there are invariant covariates 
   in the data frame.  Defaults to \code{FALSE}.
 }
 \item{\dots}{optional additional arguments to the summary functions
   that are invoked on the variables by group.  Often it is helpful to
   specify \code{na.rm = TRUE}.
 }
}

\description{
  Provide a summary of the variables in a data frame by groups of rows.
  This is most useful with a \code{groupedData} object to examine the
  variables by group.
}

\value{
  A \code{data.frame} with one row for each level of the grouping
  factor.  The number of columns is at most the number of columns in
  \code{object}.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{summary}}, \code{\link{groupedData}},
  \code{\link{getGroups}}} 

\examples{
gsummary(Orthodont)  # default summary by Subject
## gsummary with invariantsOnly = TRUE and omitGroupingFactor = TRUE
## determines whether there are covariates like Sex that are invariant
## within the repeated observations on the same Subject.
gsummary(Orthodont, inv = TRUE, omit = TRUE)
}
\keyword{manip}
