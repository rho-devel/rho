\name{G_panel.number}
\alias{current.row}
\alias{current.column}
\alias{packet.number}
\alias{panel.number}
\alias{which.packet}
\alias{trellis.currentLayout}

\title{Accessing Auxiliary Information During Plotting}

\description{

  Control over lattice plots are provided through a collection of user
  specifiable functions that perform various tasks during the plotting.
  Not all information is available to all functions.  The functions
  documented here attempt to provide a consistent interface to access
  relevant information from within these user specified functions,
  namely those specified as the \code{panel}, \code{strip} and
  \code{axis} functions.

}
\usage{

current.row(prefix)
current.column(prefix)
panel.number(prefix)
packet.number(prefix)
which.packet(prefix)

trellis.currentLayout(which = c("packet", "panel"), prefix)

}

\arguments{
  \item{which}{
    whether return value (a matrix) should contain panel numbers or
    packet numbers, which are usually, but not necessarily, the same
    (see below for details).
  }

  \item{prefix}{
    A character string acting as a prefix identifying the plot of a
    \code{"trellis"} object.  Only relevant when a particular page is
    occupied by more than one plot.  Defaults to the value appropriate
    for the last \code{"trellis"} object printed.  See
    \code{\link{trellis.focus}}.
  }

}

\value{

  \code{trellis.currentLayout} returns a matrix with as many rows and
  columns as in the layout of panels in the current plot.  Entries in
  the matrix are integer indices indicating which packet (or panel; see
  below) occupies that position, with 0 indicating the absence of a
  panel.  \code{current.row} and \code{current.column} return integer
  indices specifying which row and column in the layout are currently
  active.  \code{panel.number} returns an integer counting which panel
  is being drawn (starting from 1 for the first panel, a.k.a. the panel
  order).  \code{packet.number} gives the packet number according to the
  packet order, which is determined by varying the first conditioning
  variable the fastest, then the second, and so on.  \code{which.packet}
  returns the combination of levels of the conditioning variables in the
  form of a numeric vector as long as the number of conditioning
  variables, with each element an integer indexing the levels of the
  corresponding variable.

}

\note{

  The availability of these functions make redundant some features
  available in earlier versions of lattice, namely optional arguments
  called \code{panel.number} and \code{packet.number} that were made
  available to \code{panel} and \code{strip}.  If you have written such
  functions, it should be enough to replace instances of
  \code{panel.number} and \code{packet.number} by the corresponding
  function calls.  You should also remove \code{panel.number} and
  \code{packet.number} from the argument list of your function to avoid
  a warning.

  If these accessor functions are not enough for your needs, feel free
  to contact the maintainer and ask for more.

}

\seealso{
  \code{\link{Lattice}}, \code{\link{xyplot}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

