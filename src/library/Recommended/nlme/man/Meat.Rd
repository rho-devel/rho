% File nlme/man/Meat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Meat}
\alias{Meat}
\title{Tenderness of meat}
\description{
  The \code{Meat} data frame has 30 rows and 4 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Storage}{
      an ordered factor specifying the storage treatment - 1 (0
      days), 2 (1 day),  3 (2 days), 4 (4 days), 5 (9 days), and 6
      (18 days)
    }
    \item{score}{
      a numeric vector giving the tenderness score of beef roast.
    }
    \item{Block}{
      an ordered factor identifying the muscle from which the
      roast was extracted with levels 
      \code{II} < \code{V} < \code{I} < \code{III} < \code{IV}
    }
    \item{Pair}{
      an ordered factor giving the unique identifier for each pair
      of beef roasts with levels \code{II-1} < \dots{} < \code{IV-1}
    }
  }
}
\details{
  Cochran and Cox (section 11.51, 1957) describe data from an
  experiment conducted at Iowa State College (Paul, 1943) to
  compare the effects of length of cold storage on the tenderness of
  beef roasts.  Six storage periods ranging from 0 to 18 days were used.
  Thirty roasts were scored by four judges on a scale from 0 to 10, with
  the score increasing with tenderness. The response was the sum of all
  four scores. Left and right roasts from the same animal were grouped
  into pairs, which were further grouped into five blocks, according to
  the muscle from which they were extracted. Different storage periods
  were applied to each roast within a pair according to a balanced
  incomplete block design.}
\source{
  Cochran, W. G. and Cox, G. M. (1957), \emph{Experimental Designs},
  Wiley, New York.
}
%\examples{}
\keyword{datasets}
