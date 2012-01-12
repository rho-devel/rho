% file MASS/man/denumerate.Rd
% copyright (C) 2000 W. N. Venables and B. D. Ripley
%
\name{denumerate}
\alias{denumerate}
\alias{denumerate.formula}
\title{
Transform an Allowable Formula for 'loglm' into one for 'terms'
}
\description{
\code{\link{loglm}} allows dimension numbers to be used in place of names in
the formula.  \code{denumerate} modifies such a formula into one that
\code{\link{terms}} can process.
}
\usage{
denumerate(x)
}
\arguments{
\item{x}{
A formula conforming to the conventions of \code{\link{loglm}}, that is, it
may allow dimension numbers to stand in for names when specifying
a log-linear model.
}}
\value{
A linear model formula like that presented, except that where
dimension numbers, say \code{n}, have been used to specify fixed
margins these are replaced by names of the form \code{.vn} which may
be processed by \code{terms}.
}
\details{
The model fitting function \code{\link{loglm}} fits log-linear models to
frequency data using iterative proportional scaling.  To specify
the model the user must nominate the margins in the data that
remain fixed under the log-linear model.  It is convenient to
allow the user to use dimension numbers, 1, 2, 3, \dots for the
first, second, third, \dots, margins in a similar way to variable
names.  As the model formula has to be parsed by \code{\link{terms}}, which
treats \code{1} in a special way and requires parseable variable names,
these formulae have to be modified by giving genuine names for
these margin, or dimension numbers.  \code{denumerate} replaces these
numbers with names of a special form, namely \code{n} is replaced by
\code{.vn}.  This allows \code{terms} to parse the formula in the usual way.
}
\seealso{
\code{\link{renumerate}}
}
\examples{
denumerate(~(1+2+3)^3 + a/b)
\dontrun{~ (.v1 + .v2 + .v3)^3 + a/b}
}
\keyword{models}
