\name{poisons}
\alias{poisons}
\title{
Animal Survival Times
}
\description{
The \code{poisons} data frame has 48 rows and 3 columns.

The data form a 3x4 factorial experiment, the factors being three poisons
and four treatments.  Each combination of the two factors was used for
four animals, the allocation to animals having been completely randomized.
}
\usage{
poisons
}
\format{
This data frame contains the following columns:
\describe{
\item{\code{time}}{
The survival time of the animal in units of 10 hours.
}
\item{\code{poison}}{
A factor with levels \code{1}, \code{2} and \code{3} giving the type of poison used.
}
\item{\code{treat}}{
A factor with levels \code{A}, \code{B}, \code{C} and \code{D} giving the treatment.
}}}
\source{
The data were obtained from

Box, G.E.P. and Cox, D.R. (1964) An analysis of transformations (with
Discussion). 
\emph{ Journal of the Royal Statistical Society, B}, \bold{26}, 211--252.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
