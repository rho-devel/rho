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

    \item{\code{layout.widths}, \code{layout.heights}}{ Controls details
      of the default space allocation in the grid layout created in the
      course of plotting a \code{"trellis"} object.  Each named
      component is a list of arguments to the \pkg{grid} function
      \code{\link{unit}} (\code{x}, \code{units}, and optionally
      \code{data}).

      Usually not of interest to the end-user, who should instead use
      the similiarly named component in the graphical settings,
      modifiable using \code{\link{trellis.par.set}}.
    }

    \item{\code{drop.unused.levels}}{ A list of two components named
      \code{cond} and \code{data}, both logical flags.  The flags
      indicate whether the unused levels of factors (conditioning
      variables and primary variables respectively) will be dropped,
      which is usually relevant when a subsetting operation is performed
      or an 'interaction' is created.  See \code{\link{xyplot}} for more
      details.  Note that this does not control dropping of levels of
      the 'groups' argument.
    }

    \item{\code{legend.bbox}}{
      A character string, either \code{"full"} or \code{"panel"}.  This
      determines the interpretation of \code{x} and \code{y} when
      \code{space="inside"} in \code{key} (determining the legend; see
      \code{\link{xyplot}}): either the full figure region ('"full"'),
      or just the region that bounds the panels and strips ('"panel"').
    }

    \item{\code{default.args}}{ A list giving default values for various
      standard arguments: \code{as.table}, \code{aspect},
      \code{between}, \code{skip}, \code{strip},
      \code{xscale.components}, \code{yscale.components}, and
      \code{axis}.
    }

    \item{\code{highlight.gpar}}{ A list giving arguments to
      \code{\link{gpar}} used to highlight a viewport chosen using
      \code{\link{trellis.focus}}.
    }

    \item{\code{banking}}{ The banking function. See
      \code{\link{banking}}. }

    \item{\code{axis.padding}}{ List with components named
         \code{"numeric"} and \code{"factor"}, both scalar numbers.
         Panel limits are extended by this amount, to provide padding
         for numeric and factor scales respectively.  The value for
         numeric is multiplicative, whereas factor is additive. }

    \item{\code{skip.boundary.labels}}{ Numeric scalar between 0 and 1.
      Tick marks that are too close to the limits are not drawn unless
      explicitly requested.  The limits are contracted by this
      proportion, and anything outside is skipped. }

    \item{\code{interaction.sep}}{ The separator for creating
      interactions with the extended formula interface (see
      \code{\link{xyplot}}).
    }

    \item{\code{axis.units}}{ List determining default units for axis
      components.  Should not be of interest to the end-user.
    }

  }

  In addition, there is an option for the default prepanel and panel
  function for each high-level function; e.g., \code{panel.xyplot} and
  \code{prepanel.default.xyplot} for \code{\link{xyplot}}.  The options
  for the others have similarly patterned names.

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
str(lattice.getOption("layout.widths"), max.level = 2)
}
\keyword{dplot}


