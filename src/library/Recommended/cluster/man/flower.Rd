\name{flower}
\alias{flower}
\title{Flower Characteristics}
\usage{data(flower)}
\description{8 characteristics for 18 popular flowers.}
\format{
  A data frame with 18 observations on 8 variables:
  \tabular{rll}{
    [ , "V1"] \tab factor  \tab winters \cr
    [ , "V2"] \tab factor  \tab shadow \cr
    [ , "V3"] \tab factor  \tab tubers \cr
    [ , "V4"] \tab factor  \tab color \cr
    [ , "V5"] \tab ordered \tab soil \cr
    [ , "V6"] \tab ordered \tab preference \cr
    [ , "V7"] \tab numeric \tab height \cr
    [ , "V8"] \tab numeric \tab distance
  }

  \describe{
    \item{V1}{winters, is binary and indicates whether the plant may be left
      in the garden when it freezes.}

    \item{V2}{shadow, is binary and shows whether the plant needs to stand
      in the shadow.}

    \item{V3}{tubers, is asymmetric binary and distinguishes between plants
      with tubers and plants that grow in any other way.}

    \item{V4}{color, is nominal and specifies the flower's color (1 = white,
      2 = yellow, 3 = pink, 4 = red, 5 = blue).}

    \item{V5}{soil, is ordinal and indicates whether the plant grows in dry
      (1), normal (2), or wet (3) soil.}

    \item{V6}{preference, is ordinal and gives someone's preference ranking
      going from 1 to 18.}

    \item{V7}{height, is interval scaled, the plant's height in centimeters.}

    \item{V8}{distance, is interval scaled, the distance in centimeters that
      should be left between the plants.}
  }
}
\references{
  Struyf, Hubert and Rousseeuw (1996), see \code{\link{agnes}}.
}
\examples{
data(flower)
## Example 2 in ref
daisy(flower, type = list(asymm = 3))
daisy(flower, type = list(asymm = c(1, 3), ordratio = 7))
}
\keyword{datasets}
