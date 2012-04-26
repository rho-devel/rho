% file MASS/man/renumerate.Rd
% copyright (C) 2000 W. N. Venables and B. D. Ripley
%
\name{renumerate}
\alias{renumerate}
\alias{renumerate.formula}
\title{
Convert a Formula Transformed by 'denumerate'
}
\description{
\code{\link{denumerate}} converts a formula written using the conventions of
\code{\link{loglm}} into one that \code{\link{terms}} is able to process.  \code{renumerate}
converts it back again to a form like the original.
}
\usage{
renumerate(x)
}
\arguments{
\item{x}{
A formula, normally as modified by \code{\link{denumerate}}.
}}
\value{
A formula where all variables with names of the form \code{.vn}, where
\code{n} is an integer, converted to numbers, \code{n}, as allowed by the
formula conventions of \code{\link{loglm}}.
}
\details{
This is an inverse function to \code{\link{denumerate}}.  It is only needed
since \code{\link{terms}} returns an expanded form of the original formula
where the non-marginal terms are exposed.  This expanded form is
mapped back into a form corresponding to the one that the user
originally supplied.
}
\seealso{
\code{\link{denumerate}}
}
\examples{
denumerate(~(1+2+3)^3 + a/b)
## ~ (.v1 + .v2 + .v3)^3 + a/b
renumerate(.Last.value)
## ~ (1 + 2 + 3)^3 + a/b
}
\keyword{models}
