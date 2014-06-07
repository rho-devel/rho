\name{I_lset}
\alias{lset}
\title{Interface to modify Trellis Settings - Defunct}
\description{
  A (hopefully) simpler alternative to \code{trellis.par.get/set}.  This
  is deprecated, and the same functionality is now available with
  \code{trellis.par.set}
}

\usage{
lset(theme = col.whitebg())
}
\arguments{
  \item{theme}{ a list decribing how to change the settings of the
    current active device. Valid components are those in the list
    returned by \code{trellis.par.get()}. Each component must itself be
    a list, with one or more of the appropriate components (need not
    have all components). Changes are made to the settings for the
    currently active device only.
  }
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{utilities}
