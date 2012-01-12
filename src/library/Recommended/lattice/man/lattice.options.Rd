\name{C_04_lattice.options}
\alias{lattice.options}
\alias{lattice.getOption}
\title{ Low-level Options Controlling Behaviour of Lattice }
\description{
  Functions to handle settings used by lattice.  Their main purpose is
  to make code maintainance easier, and users normally should not need
  to use these functions.  However, fine control at this level maybe
  useful in certain cases.
}

\usage{
lattice.options(\dots)
lattice.getOption(name)
}
\arguments{
  \item{name}{ character giving the name of a setting }
  \item{\dots}{
    new options can be defined, or existing ones modified, using one or
    more arguments of the form \code{name = value} or by passing a list
    of such tagged values.  Existing values can be retrieved by
    supplying the names (as character strings) of the components as
    unnamed arguments.
  }
}
\details{

  These functions are modeled on \code{options} and \code{getOption},
  and behave similarly for the most part.  Some of the available
  components are documented here, but not all.  The purpose of the ones
  not documented are either fairly obvious, or not of interest to the
  end-user.

  \describe{

    \item{\code{panel.error}}{
      A function, or \code{NULL}.  If the former, every call to the
      panel function will be wrapped inside \code{\link{tryCatch}} with
      the specified function as an error handler.  The default is to use
      the \code{\link{panel.error}} function.  This prevents the plot
      from failing due to errors in a single panel, and leaving the grid
      operations in an unmanageable state.  If set to \code{NULL},
      errors in panel functions will not be caught using
      \code{tryCatch}.
    }

    \item{\code{save.object}}{
      Logical flag indicating whether a \code{"trellis"} object should
      be saved when plotted for subsequent retrieval and further
      manipulation.  Defaults to \code{TRUE}.
    }

    \item{\code{layout.widths}, \code{layout.heights}}{
      Controls details of the default space allocation in the grid
      layout created in the course of plotting a \code{"trellis"}
      object.  Not of interest to the end-user, who should instead use
      the similiarly named component in the graphical settings,
      modifiable using \code{\link{trellis.par.set}}.
    }

    \item{\code{drop.unused.levels}}{ }

    \item{\code{legend.bbox}}{}


  }

}

\value{
  \code{lattice.getOption} returns the value of a single component,
  whereas \code{lattice.options} always returns a list with one or more
  named components.  When changing the values of components, the old
  values of the modified components are returned by
  \code{lattice.options}.  If called without any arguments, the full
  list is returned. 
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\seealso{
  \code{\link{options}}, \code{\link{trellis.device}},
  \code{\link{trellis.par.get}}, \code{\link{Lattice}}
}

\examples{
names(lattice.options())
str(lattice.getOption("layout.widths"))
}
\keyword{dplot}


