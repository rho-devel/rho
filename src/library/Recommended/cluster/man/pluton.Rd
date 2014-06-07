\name{pluton}
\alias{pluton}
\title{Isotopic Composition Plutonium Batches}
\usage{data(pluton)}
\description{
  The \code{pluton} data frame has 45 rows and 4 columns,
  containing percentages of isotopic composition of 45 Plutonium
  batches.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Pu238}{the percentages of \eqn{\ ^{238}Pu}{(238)Pu},
      always less than 2 percent.}
    \item{Pu239}{the percentages of \eqn{\ ^{239}Pu}{(239)Pu},
      typically between 60 and 80 percent (from neutron capture of Uranium,
      \eqn{\ ^{238}U}{(238)U}).}
    \item{Pu240}{percentage of the plutonium 240 isotope.}
    \item{Pu241}{percentage of the plutonium 241 isotope.}
  }
}
\details{
  Note that the percentage of plutonium~242 can be computed from the
  other four percentages, see the examples.

  In the reference below it is explained why it is very desirable to
  combine these plutonium patches in three groups of similar size.
}
\source{
  Available as \file{pluton.dat} from the archive
  %%   originally at
  %%   \url{http://win-www.uia.ac.be/u/statis/datasets/clusplot-examples.tar.gz},
  currently \url{http://www.agoras.ua.ac.be/datasets/clusplot-examples.tar.gz}.
}
\references{
  Rousseeuw, P.J. and Kaufman, L and Trauwaert, E. (1996)
  Fuzzy clustering using scatter matrices,
  \emph{Computational Statistics and Data Analysis} \bold{23}(1), 135--151.
}
\examples{
data(pluton)

hist(apply(pluton,1,sum), col = "gray") # between 94\% and 100\%
pu5 <- pluton
pu5$Pu242 <- 100 - apply(pluton,1,sum) # the remaining isotope.
pairs(pu5)
}
\keyword{datasets}
