% File nlme/man/bdf.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{bdf}
\alias{bdf}
\title{Language scores}
\usage{data(bdf)}
\description{
The \code{bdf} data frame has 2287 rows and 25 columns of language
scores from grade 8 pupils in elementary schools in The Netherlands.
}
\format{
  \describe{
    \item{schoolNR}{a factor denoting the school.}
    \item{pupilNR}{a factor denoting the pupil.}
    \item{IQ.verb}{a numeric vector of verbal IQ scores}
    \item{IQ.perf}{a numeric vector of IQ scores.}
    \item{sex}{Sex of the student.}
    \item{Minority}{a factor indicating if the student is a member of a
      minority group.}
    \item{repeatgr}{an ordered factor indicating if one or more grades
      have been repeated.}
    \item{aritPRET}{a numeric vector}
    \item{classNR}{a numeric vector}
    \item{aritPOST}{a numeric vector}
    \item{langPRET}{a numeric vector}
    \item{langPOST}{a numeric vector}
    \item{ses}{a numeric vector of socioeconomic status indicators.}
    \item{denomina}{a factor indicating of the school is a public
      school, a Protestant private school, a Catholic private school, or
      a non-denominational private school.}
    \item{schoolSES}{a numeric vector}
    \item{satiprin}{a numeric vector}
    \item{natitest}{a factor with levels \code{0} and \code{1}}
    \item{meetings}{a numeric vector}
    \item{currmeet}{a numeric vector}
    \item{mixedgra}{a factor indicating if the class is a mixed-grade class.}
    \item{percmino}{a numeric vector}
    \item{aritdiff}{a numeric vector}
    \item{homework}{a numeric vector}
    \item{classsiz}{a numeric vector}
    \item{groupsiz}{a numeric vector}
  }
}
\source{
  \url{http://stat.gamma.rug.nl/snijders/multilevel.htm}
}
\references{
  Snijders, Tom and Bosker, Roel (1999), \emph{Multilevel Analysis: An
    Introduction to Basic and Advanced Multilevel Modeling}, Sage.
}
\examples{
summary(bdf)
}
\keyword{datasets}
