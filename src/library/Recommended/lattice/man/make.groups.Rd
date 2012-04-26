\name{D_make.groups}
\alias{make.groups}
\title{ Grouped data from multiple vectors }
\description{
  Combines two or more vectors, possibly of different lengths, producing
  a data frame with a second column indicating which of these vectors
  that row came from.  This is mostly useful for getting data into a
  form suitable for use in high level Lattice functions.
}
\usage{
make.groups(\dots)
}
\arguments{
  \item{\dots}{
    one or more vectors of the same type (coercion is attempted if not),
    or one or more data frames with similar columns, with possibly
    differing number of rows.
  }
}
\value{
  When all the input arguments are vectors, a data frame with two
  columns
  \item{\code{data}}{
    all the vectors supplied, concatenated
  }
  \item{\code{which}}{
    factor indicating which vector the corresponding \code{data} value
    came from
  }
  When all the input arguments are data frames, the result of
  \code{\link{rbind}} applied to them, along with an additional
  \code{which} column as described above.
}

\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{Lattice}}}
\examples{
sim.dat <-
    make.groups(uniform = runif(200),
                exponential = rexp(175),
                lognormal = rlnorm(150),
                normal = rnorm(125))
qqmath( ~ data | which, sim.dat, scales = list(y = "free"))
}
\keyword{dplot}

