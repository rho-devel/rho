% File nlme/man/collapse.groupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: collapse.groupedData.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{collapse.groupedData}
\title{Collapse a groupedData Object}
\usage{
\method{collapse}{groupedData}(object, collapseLevel, displayLevel,
       outer, inner, preserve, FUN, subset, \dots)
}
\alias{collapse.groupedData}
\arguments{
 \item{object}{an object inheriting from class \code{groupedData},
   generally with multiple grouping factors.
 }
 \item{collapseLevel}{an optional positive integer or character string
   indicating the grouping level to use when collapsing the data. Level
   values increase from outermost to innermost grouping. Default is the
   highest or innermost level of grouping.
 }
 \item{displayLevel}{an optional positive integer or character string
   indicating the grouping level to use as the grouping factor for the
   collapsed data. Default is \code{collapseLevel}.
 }
 \item{outer}{an optional logical value or one-sided formula,
   indicating covariates that are outer to the \code{displayLevel}
   grouping factor. If equal to \code{TRUE}, the \code{displayLevel}
   element \code{attr(object, "outer")} is used to indicate the 
   outer covariates. An outer covariate is invariant within the sets
   of rows defined by the grouping factor.  Ordering of the groups is
   done in such a way as to preserve adjacency of groups with the same
   value of the outer variables. Defaults to \code{NULL}, meaning that
   no outer covariates are to be used.
 }  
 \item{inner}{an optional logical value or one-sided formula, indicating
   a covariate that is inner to the \code{displayLevel} grouping
   factor. If equal to \code{TRUE}, \code{attr(object, "outer")} is used
   to indicate the inner covariate. An inner covariate can change within
   the sets of rows defined by the grouping  factor. Defaults to
   \code{NULL}, meaning that no inner covariate is present.  
 } 
 \item{preserve}{an optional one-sided formula indicating a covariate
   whose levels should be preserved when collapsing the data according
   to the \code{collapseLevel} grouping factor. The collapsing factor is
   obtained by pasting together the levels of the \code{collapseLevel}
   grouping factor and the values of the covariate to be
   preserved. Default is \code{NULL}, meaning that no covariates need to
   be preserved.
 }
  \item{FUN}{an optional summary function or a list of summary functions
   to be used for collapsing the data.  The function or functions are
   applied only to variables in \code{object} that vary within the
   groups defined by \code{collapseLevel}.  Invariant variables are 
   always summarized by group using the unique value that they assume
   within that group.  If \code{FUN} is a single
   function it will be applied to each non-invariant variable by group
   to produce the summary for that variable.  If \code{FUN} is a list of
   functions, the names in the list should designate classes of
   variables in the data such as \code{ordered}, \code{factor}, or
   \code{numeric}.  The indicated function will be applied to any
   non-invariant variables of that class.  The default functions to be
   used are \code{mean} for numeric factors, and \code{Mode} for both
   \code{factor} and \code{ordered}.  The \code{Mode} function, defined
   internally in \code{gsummary}, returns the modal or most popular
   value of the variable.  It is different from the \code{mode} function
   that returns the S-language mode of the variable.}
 \item{subset}{an optional named list. Names can be either positive
   integers representing grouping levels, or names of grouping
   factors. Each element in the list is a vector indicating the levels
   of the corresponding grouping factor to be preserved in the collapsed
   data. Default is \code{NULL}, meaning that all levels are
   used.
 } 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
 If \code{object} has a single grouping factor, it is returned
 unchanged. Else, it is summarized by the values of the
 \code{displayLevel} grouping factor (or the combination of its values
 and the values of the covariate indicated in \code{preserve}, if any is
 present). The collapsed data is used to produce a new
 \code{groupedData} object, with grouping factor given by the
 \code{displayLevel} factor. 
}
\value{
  a \code{groupedData} object with a single grouping factor given by the
  \code{displayLevel} grouping factor, resulting from collapsing
  \code{object} over the levels of the \code{collapseLevel} grouping
  factor. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{groupedData}}, \code{\link{plot.nmGroupedData}}
}  

\examples{
# collapsing by Dog
collapse(Pixel, collapse = 1)  # same as collapse(Pixel, collapse = "Dog")
}
\keyword{models}
