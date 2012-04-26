% File nlme/man/groupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: groupedData.Rd,v 1.8 2002/01/28 03:44:17 bates Exp $
\name{groupedData}
\title{Construct a groupedData Object}
\usage{
groupedData(formula, data, order.groups, FUN, outer, inner,
 labels, units)
\method{update}{groupedData}(object, formula, data, order.groups, FUN,
outer, inner, labels, units, \dots)
}
\alias{groupedData}
\alias{nfGroupedData}
\alias{nmGroupedData}
\alias{[.groupedData}
\alias{as.data.frame.groupedData}
\alias{update.groupedData}
\arguments{
 \item{object}{an object inheriting from class \code{groupedData}.}
 \item{formula}{a formula of the form \code{resp ~ cov | group} where
   \code{resp} is the response, \code{cov} is the primary covariate, and
   \code{group} is the grouping factor.  The expression \code{1} can be
   used for the primary covariate when there is no other suitable
   candidate.  Multiple nested grouping factors can be listed separated
   by the \code{/} symbol as in \code{fact1/fact2}.  In an expression
   like this the \code{fact2} factor is nested within the \code{fact1}
   factor.}
 \item{data}{a data frame in which the expressions in \code{formula} can
   be evaluated.  The resulting \code{groupedData} object will consist
   of the same data values in the same order but with additional
   attributes.}
 \item{order.groups}{an optional logical value, or list of logical
   values, indicating if the grouping factors should be converted to
   ordered factors according to the function \code{FUN} applied to the
   response from each group. If multiple levels of grouping are present,
   this argument can be either a single logical value (which will be
   repeated for all grouping levels) or a list of logical values. If no
   names are assigned to the list elements, they are assumed in the same
   order as the group levels (outermost to innermost grouping). Ordering
   within a level of grouping is done within the levels of the grouping
   factors which are outer to it. Changing the grouping factor to an
   ordered factor does not affect the ordering of the rows in the data
   frame but it does affect the order of the panels in a trellis display
   of the data or models fitted to the data.  Defaults to \code{TRUE}.}  
 \item{FUN}{an optional summary function that will be applied to the
   values of the response for each level of the grouping factor, when
   \code{order.groups = TRUE}, to determine the ordering.  Defaults to
   the \code{max} function.}
 \item{outer}{an optional one-sided formula, or list of one-sided
   formulas, indicating covariates that are outer to the grouping
   factor(s).  If multiple levels of grouping are present,
   this argument can be either a single one-sided formula, or a list of
   one-sided formulas. If no names are assigned to the list elements,
   they are assumed in the same order as the group levels (outermost to
   innermost grouping). An outer covariate is invariant within the sets
   of rows defined by the grouping factor.  Ordering of the groups is
   done in such a way as to preserve adjacency of groups with the same
   value of the outer variables.  When plotting a  groupedData object,
   the argument \code{outer = TRUE} causes the panels to be determined
   by the \code{outer} formula.  The points within the panels are 
   associated by level of the grouping factor. Defaults to \code{NULL},
   meaning that no outer covariates are present.} 
 \item{inner}{an optional one-sided formula, or list of one-sided
   formulas, indicating covariates that are inner to the grouping
   factor(s). If multiple levels of grouping are present,
   this argument can be either a single one-sided formula, or a list of
   one-sided formulas. If no names are assigned to the list elements,
   they are assumed in the same order as the group levels (outermost to
   innermost grouping). An inner covariate can change 
   within the sets of rows defined by the grouping factor.  An inner
   formula can be used to associate points in a plot of a groupedData
   object.  Defaults to \code{NULL}, meaning that no inner covariates
   are present.} 
 \item{labels}{an optional list of character strings giving labels for
   the response and the primary covariate.  The label for the primary
   covariate is named \code{x} and that for the response is named
   \code{y}.  Either label can be omitted.}
 \item{units}{an optional list of character strings giving the units for
   the response and the primary covariate.  The units string for the
   primary covariate is named \code{x} and that for the response is
   named \code{y}.  Either units string can be omitted.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  An object of the \code{groupedData} class is constructed from the
  \code{formula} and \code{data} by attaching the \code{formula} as an
  attribute of the data, along with any of \code{outer}, \code{inner},
  \code{labels}, and \code{units} that are given.  If
  \code{order.groups} is \code{TRUE} the grouping factor is converted to
  an ordered factor with the ordering determined by
  \code{FUN}. Depending on the number of grouping levels and the type of
  primary covariate, the returned object will be of one of three
  classes: \code{nfnGroupedData} - numeric covariate, single level of
  nesting; \code{nffGroupedData} - factor covariate, single level of
  nesting; and \code{nmGroupedData} - multiple levels of
  nesting. Several modeling and plotting functions can use the formula
  stored with a \code{groupedData} object to construct default plots and
  models. 
}
\value{
  an object of one of the classes \code{nfnGroupedData},
  \code{nffGroupedData}, or \code{nmGroupedData}, and also inheriting
  from  classes \code{groupedData} and \code{data.frame}. 
}
\references{
  Bates, D.M. and Pinheiro, J.C. (1997), "Software Design for Longitudinal
  Data", in "Modelling Longitudinal and Spatially Correlated Data:
  Methods, Applications and Future Directions", T.G. Gregoire (ed.),
  Springer-Verlag, New York.

  Pinheiro, J.C. and Bates, D.M. (1997) "Future Directions in
  Mixed-Effects Software: Design of NLME 3.0" available at
  http://nlme.stat.wisc.edu/

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}

\author{Douglas Bates and Jose Pinheiro}

\seealso{\code{\link{formula}}, \code{\link{gapply}},
  \code{\link{gsummary}},
  \code{\link{lme}},
  \code{\link{plot.nffGroupedData}},
  \code{\link{plot.nfnGroupedData}},
  \code{\link{plot.nmGroupedData}},
  \code{\link{reStruct}}
}
\examples{
Orth.new <-  # create a new copy of the groupedData object
  groupedData( distance ~ age | Subject,
              data = as.data.frame( Orthodont ),
              FUN = mean,
              outer = ~ Sex,
              labels = list( x = "Age",
                y = "Distance from pituitary to pterygomaxillary fissure" ),
              units = list( x = "(yr)", y = "(mm)") )
\dontrun{
plot( Orth.new )         # trellis plot by Subject
}
formula( Orth.new )      # extractor for the formula
gsummary( Orth.new )     # apply summary by Subject
fm1 <- lme( Orth.new )   # fixed and groups formulae extracted from object
Orthodont2 <- update(Orthodont, FUN = mean)
}
\keyword{manip}
\keyword{attribute}
