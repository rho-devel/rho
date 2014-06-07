\name{F_2_panel.loess}
\alias{panel.loess}
\title{Panel Function to Add a LOESS Smooth}
\description{
  A predefined panel function that can be used to add a LOESS smooth
  based on the provided data.
}
\usage{

panel.loess(x, y, span = 2/3, degree = 1,
            family = c("symmetric", "gaussian"),
            evaluation = 50,
            lwd, lty, col, col.line, type,
            horizontal = FALSE,
            \dots, identifier = "loess")
}
\arguments{
  \item{x, y}{ Variables defining the data to be used. }

  \item{lwd, lty, col, col.line}{ Graphical parameters for the added
    line.  \code{col.line} overrides \code{col}. }

  \item{type}{ Ignored.  The argument is present only to make sure that
    an explicitly specified \code{type} argument (perhaps meant for
    another function) does not affect the display.  }

  \item{span, degree, family, evaluation}{ Arguments to
    \code{\link{loess.smooth}}, for which \code{panel.loess} is
    essentially a wrapper.
  }
  \item{horizontal}{
    A logical flag controlling which variable is to be treated as the
    predictor (by default \code{x}) and which as the response (by
    default \code{y}).  If \code{TRUE}, the plot is \sQuote{transposed}
    in the sense that \code{y} becomes the predictor and \code{x} the
    response.  (The name \sQuote{horizontal} may seem an odd choice for
    this argument, and originates from similar usage in
    \code{\link{bwplot}}).
  }
  \item{\dots}{ Extra arguments, passed on to
    \code{\link{panel.lines}}.
  }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\value{
  The object returned by \code{\link[stats:scatter.smooth]{loess.smooth}}.
}

\seealso{\link{Lattice},
  \code{\link[stats:scatter.smooth]{loess.smooth}},
  \code{\link{prepanel.loess}}  
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

