\name{A_01_Lattice}
\alias{Lattice}
\alias{lattice}
\alias{lattice-package}
\title{Lattice Graphics}
\description{

  The \pkg{lattice} add-on package is an implementation of Trellis
  graphics for \R.  It is a powerful and elegant high-level data
  visualization system with an emphasis on multivariate data.  It is
  designed to meet most typical graphics needs with minimal tuning, but
  can also be easily extended to handle most nonstandard requirements.

}
\details{

  Trellis Graphics, originally developed for S and S-PLUS at the Bell
  Labs, is a framework for data visualization developed by R. A. Becker,
  W. S. Cleveland, et al, extending ideas presented in Cleveland's 1993
  book \emph{Visualizing Data}.  The Lattice API is based on the
  original design in S, but extends it in many ways.

  The Lattice user interface primarily consists of several
  \sQuote{high-level} generic functions (listed below in the \dQuote{See
  Also} section), each designed to create a particular type of display
  by default.  Although the functions produce different output, they
  share many common features, reflected in several common arguments that
  affect the resulting displays in similar ways.  These arguments are
  extensively (sometimes only) documented in the help page for
  \code{\link{xyplot}}, which also includes a discussion of the
  important topics of \emph{conditioning} and control of the Trellis
  layout.  Features specific to other high-level functions are
  documented in their respective help pages.

  Lattice employs an extensive system of user-controllable settings to
  determine the look and feel of the displays it produces.  To learn how
  to use and customize the graphical parameters used by lattice, see
  \code{\link{trellis.par.set}}.  For other settings, see
  \code{\link{lattice.options}}.  The default graphical settings are
  (potentially) different for different graphical devices.  To learn how
  to initialize new devices with the desired settings or change the
  settings of the current device, see \code{\link{trellis.device}}.

  It is usually unnecessary, but sometimes important to be able to plot
  multiple lattice plots on a single page.  Such capabilities are
  described in the \code{\link{print.trellis}} help page.  See
  \code{\link{update.trellis}} to learn about manipulating a
  \code{"trellis"} object.  Tools to augment lattice plots after they
  are drawn (including \code{\link{locator}}-like functionality) are
  described in the \code{\link{trellis.focus}} help page.

  The online documentation accompanying the package is complete, and
  effort has been made to present the help pages in a logical sequence,
  so that one can learn how to use lattice by reading the PDF reference
  manual available at
  \url{http://cran.r-project.org/web/packages/lattice/}.  However, the
  format in which the online documentation is written and the breadth of
  topics covered necessarily makes it somewhat terse and less than ideal
  as a first introduction.  For a more gentle introduction, a book on
  lattice is available as part of Springer's \sQuote{Use R} series; see
  the \dQuote{References} section below.

  % Furthermore, short vignettes focusing on particular topics are
  % available from the Lattice website at
  % \url{http://lattice.r-forge.r-project.org/}

}

\note{

  High-level \pkg{lattice} functions like \code{\link{xyplot}} are
  different from traditional \R graphics functions in that they do not
  perform any plotting themselves.  Instead, they return an object, of
  class \code{"trellis"}, which has to be then
  \code{\link[lattice:print.trellis]{print}}-ed or
  \code{\link[lattice:print.trellis]{plot}}-ted to create the actual
  plot.  Due to \R's automatic printing rule, it is usually not
  necessary to explicitly carry out the second step, and \pkg{lattice}
  functions appear to behave like their traditional counterparts.
  However, the automatic plotting is suppressed when the high-level
  functions are called inside another function (most often
  \code{source}) or in other contexts where automatic printing is
  suppressed (e.g., \code{\link{for}} or \code{\link{while}} loops).  In
  such situations, an explicit call to \code{print} or \code{plot} is
  required.

  The \pkg{lattice} package is based on the Grid graphics engine and
  requires the \pkg{grid} add-on package.  One consquence of this is
  that it is not (readily) compatible with traditional \R graphics
  tools.  In particular, changing \code{par()} settings usually has no
  effect on Lattice plots; \pkg{lattice} provides its own interface for
  querying and modifying an extensive set of graphical and non-graphical
  settings.

}

\references{

  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
    Visualization with R}, Springer.  ISBN: 978-0-387-75968-5
  \url{http://lmdvr.r-forge.r-project.org/}

  Cleveland, William .S. (1993) \emph{Visualizing Data}, Hobart Press,
  Summit, New Jersey.

  Becker, R. A. and Cleveland, W. S. and Shyu, M. J. (1996).  \dQuote{The
  Visual Design and Control of Trellis Display}, \emph{Journal of
  Computational and Graphical Statistics}, \bold{5(2)}, 123--155.

  Bell Lab's Trellis Page contains several documents outlining the use
  of Trellis graphics; these provide a holistic introduction to
  the Trellis paradigm:
  \url{http://cm.bell-labs.com/cm/ms/departments/sia/project/trellis/}
  
}

\seealso{

  The following is a list of high-level functions in the \pkg{lattice}
  package and their default displays.  In all cases, the actual display
  is produced by the so-called \dQuote{panel} function, which has a
  suitable default, but can be substituted by an user defined function
  to create customized displays.  In many cases, the default panel
  function will itself have many optional arguments to customize its
  output.  The default panel functions are named as
  \dQuote{\code{panel.}} followed by the name of the corresponding
  high-level function; i.e., the default panel function for
  \code{\link{xyplot}} is \code{\link{panel.xyplot}}, the one for
  \code{\link{histogram}} is \code{\link{panel.histogram}}, etc.  Each
  default panel function has a separate help page, linked from the help
  pages of the corresponding high-level function.  Although documented
  separately, arguments to these panel functions can be supplied
  directly to the high-level functions, which will pass on the arguments
  appropriately.

  \bold{Univariate:}
  \describe{
    \item{\code{\link{barchart}}:}{ Bar plots. }
    \item{\code{\link{bwplot}}:}{ Box-and-whisker plots. }
    \item{\code{\link{densityplot}}:}{ Kernel density estimates. }
    \item{\code{\link{dotplot}}:}{ Cleveland dot plots. }
    \item{\code{\link{histogram}}:}{ Histograms. }
    \item{\code{\link{qqmath}}:}{ Theretical quantile plots. }
    \item{\code{\link{stripplot}}:}{ One-dimensional scatterplots. }
  }

  \bold{Bivariate:}
  \describe{
    \item{\code{\link{qq}}:}{ Quantile plots for comparing two distributions. }
    \item{\code{\link{xyplot}}:}{ Scatterplots and time-series plots (and potentially a lot more). }
  }

  \bold{Trivariate:}
  \describe{
    \item{\code{\link{levelplot}}:}{ Level plots (similar to \code{\link{image}} plots). }
    \item{\code{\link{contourplot}}:}{ Contour plots. }
    \item{\code{\link{cloud}}:}{ Three-dimensional scatter plots. }
    \item{\code{\link{wireframe}}:}{ Three-dimensional surface plots (similar to \code{\link{persp}} plots). }
  }
  
  \bold{Hypervariate:}
  \describe{
    \item{\code{\link{splom}}:}{ Scatterplot matrices. }
    \item{\code{\link{parallel}}:}{ Parallel coordinate plots. }
  }
  
  \bold{Miscellaneous:}
  \describe{
    \item{\code{\link{rfs}}:}{ Residual and fitted value plots (also see \code{\link{oneway}}). }
    \item{\code{\link{tmd}}:}{ Tukey Mean-Difference plots. }
  }

  In addition, there are several panel functions that do little by
  themselves, but can be useful components of custom panel functions.
  These are documented in \code{\link{panel.functions}}.  Lattice also
  provides a collection of convenience functions that correspond to the
  traditional graphics primitives \code{\link{lines}},
  \code{\link{points}}, etc.  These are implemented using Grid graphics,
  but try to be as close to the traditional versions as possible in
  terms of their argument list.  These functions have names like
  \code{\link{llines}} or \code{\link{panel.lines}} and are often useful
  when writing (or porting from S-PLUS code) nontrivial panel functions.

  Finally, many useful enhancements that extend the Lattice system are
  available in the \pkg{latticeExtra} package.

}

\examples{
\dontrun{

## Show brief history of changes to lattice, including
## a summary of new features.

RShowDoc("NEWS", package = "lattice")
}
}

\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
