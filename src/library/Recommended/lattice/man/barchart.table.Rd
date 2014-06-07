\name{B_02_barchart.table}
\title{table methods for barchart and dotplot}
\alias{barchart.table}
\alias{barchart.array}
\alias{barchart.matrix}
\alias{dotplot.table}
\alias{dotplot.array}
\alias{dotplot.matrix}

\usage{
\method{barchart}{table}(x, data, groups = TRUE,
         origin = 0, stack = TRUE, \dots, horizontal = TRUE)

\method{barchart}{array}(x, data, \dots)

\method{barchart}{matrix}(x, data, \dots)

\method{dotplot}{table}(x, data, groups = TRUE, \dots, horizontal = TRUE)

\method{dotplot}{array}(x, data, \dots)

\method{dotplot}{matrix}(x, data, \dots)
}

\description{

  Contingency tables are often displayed using bar charts and dot plots.
  These methods operate directly on tables, bypassing the need to
  convert them to data frames for use with the formula interface.
  Matrices and arrays are also supported, by coercing them to tables.

}

\arguments{
  \item{x}{
    A \code{table}, \code{array} or \code{matrix} object.
  }
  \item{data}{
    Should not be specified. If specified, will be ignored with a
    warning.
  }
  \item{groups}{
    A logical flag, indicating whether to use the last dimension as a
    grouping variable in the display.
  }
  \item{origin, stack}{
    Arguments to \code{\link{panel.barchart}}.  The defaults for the
    \code{table} method are different.
  }
  \item{horizontal}{
    Logical flag, indicating whether the plot should be horizontal (with
    the categorical variable on the y-axis) or vertical.
  }
  \item{\dots}{
    Other arguments, passed to the underlying \code{formula} method.
  }
}
  
\details{

  The first dimension is used as the variable on the categorical axis.
  The last dimension is optionally used as a grouping variable (to
  produce stacked barcharts by default).  All other dimensions are used
  as conditioning variables.  The order of these variables cannot be
  altered (except by permuting the original argument beforehand using
  \code{\link{t}} or \code{\link{aperm}}).  For more flexibility, use
  the formula method after converting the table to a data frame using
  the relevant \code{\link[base:table]{as.data.frame}} method.

}

\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}

\seealso{

  \code{\link{barchart}}, \code{\link{t}}, \code{\link{aperm}},
  \code{\link{table}}, \code{\link{panel.barchart}},
  \code{\link{Lattice}}

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
barchart(Titanic, scales = list(x = "free"),
         auto.key = list(title = "Survived"))
}
\keyword{hplot}
