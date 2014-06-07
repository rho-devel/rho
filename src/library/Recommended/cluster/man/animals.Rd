\name{animals}
\alias{animals}
\title{Attributes of Animals}
\usage{data(animals)}
\description{
  This data set considers 6 binary attributes for 20 animals.
}
\format{
  A data frame with 20 observations on 6 variables:
  \tabular{rll}{
    [ , 1] \tab war \tab warm-blooded \cr
    [ , 2] \tab fly \tab can fly \cr
    [ , 3] \tab ver \tab vertebrate \cr
    [ , 4] \tab end \tab endangered \cr
    [ , 5] \tab gro \tab live in groups \cr
    [ , 6] \tab hai \tab have hair \cr
  }
  All variables are encoded as 1 = `no', 2 = `yes'.
}
\source{
  Leonard Kaufman and Peter J. Rousseeuw (1990):
  \emph{Finding Groups in Data}
  (pp 297ff).
  New York: Wiley.
}
\details{
  This dataset is useful for illustrating monothetic (only a single
  variable is used for each split) hierarchical clustering.
}
\references{
  see Struyf, Hubert & Rousseeuw (1996),  in \code{\link{agnes}}.
}
\examples{
data(animals)
apply(animals,2, table) # simple overview

ma <- mona(animals)
ma
## Plot similar to Figure 10 in Struyf et al (1996)
plot(ma)
}
\keyword{datasets}
