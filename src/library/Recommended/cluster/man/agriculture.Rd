\name{agriculture}
\alias{agriculture}
\title{European Union Agricultural Workforces}
\usage{data(agriculture)}
\description{
  Gross National Product (GNP) per capita and percentage of the
  population working in agriculture for each country belonging to the
  European Union in 1993.
}
\format{
  A data frame with 12 observations on 2 variables:
  \tabular{rlll}{
    [ , 1] \tab \code{x} \tab numeric \tab per capita GNP \cr
    [ , 2] \tab \code{y} \tab numeric \tab percentage in agriculture
  }
  The row names of the data frame indicate the countries.
}
\source{
  Eurostat (European Statistical Agency, 1994):
  \emph{Cijfers en feiten: Een statistisch portret van de Europese Unie}.
}
\details{
  The data seem to show two clusters, the \dQuote{more agricultural} one
  consisting of Greece, Portugal, Spain, and Ireland.
}
\seealso{\code{\link{agnes}}, \code{\link{daisy}}, \code{\link{diana}}.
}
\references{
  see those in \code{\link{agnes}}.
}
\examples{
data(agriculture)

## Compute the dissimilarities using Euclidean metric and without
## standardization
daisy(agriculture, metric = "euclidean", stand = FALSE)

## 2nd plot is similar to Figure 3 in Struyf et al (1996)
plot(pam(agriculture, 2))

## Plot similar to Figure 7 in Struyf et al (1996)
\dontrun{plot(agnes(agriculture), ask = TRUE)}
\dontshow{plot(agnes(agriculture))}

## Plot similar to Figure 8 in Struyf et al (1996)
\dontrun{plot(diana(agriculture), ask = TRUE)}
\dontshow{plot(diana(agriculture))}
}
\keyword{datasets}
