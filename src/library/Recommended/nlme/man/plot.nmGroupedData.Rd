% File nlme/man/plot.nmGroupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: plot.nmGroupedData.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{plot.nmGroupedData}
\title{Plot an nmGroupedData Object}
\usage{
\method{plot}{nmGroupedData}(x, collapseLevel, displayLevel, outer, inner,
    preserve, FUN, subset, key, grid, \dots)
}
\alias{plot.nmGroupedData}
\arguments{
 \item{x}{an object inheriting from class \code{nmGroupedData},
   representing a \code{groupedData} object with multiple grouping
   factors.
 }
 \item{collapseLevel}{an optional positive integer or character string
   indicating the grouping level to use when collapsing the data. Level
   values increase from outermost to innermost grouping. Default is the
   highest or innermost level of grouping.
 }
 \item{displayLevel}{an optional positive integer or character string
   indicating the grouping level to use for determining the panels in
   the Trellis display, when \code{outer} is missing. Default is
   \code{collapseLevel}.
 }
 \item{outer}{an optional logical value or one-sided formula,
   indicating covariates that are outer to the \code{displayLevel}
   grouping factor, which are used to determine the panels of the
   Trellis plot. If equal to \code{TRUE}, the \code{displayLevel}
   element \code{attr(object, "outer")} is used to indicate the 
   outer covariates. An outer covariate is invariant within the sets
   of rows defined by the grouping factor.  Ordering of the groups is
   done in such a way as to preserve adjacency of groups with the same
   value of the outer variables. Defaults to \code{NULL}, meaning that
   no outer covariates are to be used.
 }  
 \item{inner}{an optional logical value or one-sided formula, indicating
   a covariate that is inner to the \code{displayLevel} grouping factor,
   which is used to associate points within each panel of the Trellis
   plot. If equal to \code{TRUE}, \code{attr(object, "outer")} is used
   to indicate the inner covariate. An inner covariate can change within
   the sets of rows defined by the grouping  factor. Defaults to
   \code{NULL}, meaning that no inner covariate  is present.  
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
   of the corresponding grouping factor to be used for plotting the
   data. Default is \code{NULL}, meaning that all levels are
   used.} 
 \item{key}{an optional logical value, or list. If \code{TRUE}, a legend
   is included at the top of the plot indicating which symbols (colors)
   correspond to which prediction levels. If \code{FALSE}, no legend
   is included. If given as a list, \code{key} is passed down as an
   argument to the \code{trellis} function generating the plots
   (\code{xyplot}). Defaults to \code{TRUE}.}
 \item{grid}{an optional logical value indicating whether a grid should
   be added to plot. Default is \code{TRUE}.}
 \item{\dots}{optional arguments passed to the Trellis plot function.}
}
\description{
 The \code{groupedData} object is summarized by the values of the
 \code{displayLevel} grouping factor (or the combination of its values
 and the values of the covariate indicated in \code{preserve}, if any is
 present). The collapsed data is used to produce a new
 \code{groupedData} object, with grouping factor given by the
 \code{displayLevel} factor, which is plotted using the
 appropriate \code{plot} method for \code{groupedData} objects with
 single level of grouping. 
}
\value{
  a Trellis display of the data collapsed over the values of the
  \code{collapseLevel} grouping factor and grouped according to the
  \code{displayLevel} grouping factor. 
}

\references{
Bates, D.M. and Pinheiro, J.C. (1997), "Software Design for Longitudinal
Data", in "Modelling Longitudinal and Spatially Correlated Data:
Methods, Applications and Future Directions", T.G. Gregoire (ed.),
Springer-Verlag, New York.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{groupedData}}, \code{\link{collapse.groupedData}},
  \code{\link{plot.nfnGroupedData}}, \code{\link{plot.nffGroupedData}}
}  
\examples{
# no collapsing, panels by Dog
plot(Pixel, display = "Dog", inner = ~Side)
# collapsing by Dog, preserving day
plot(Pixel, collapse = "Dog", preserve = ~day)
}
\keyword{models}
