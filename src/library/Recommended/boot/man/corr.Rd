\name{corr}
\alias{corr}
\title{
Correlation Coefficient
}
\description{
Calculates the weighted correlation given a data set and a set of weights.
}
\usage{
corr(d, w = rep(1, nrow(d))/nrow(d))
}
\arguments{
\item{d}{
A matrix with two columns corresponding to the two variables whose correlation
we wish to calculate.
}
\item{w}{
A vector of weights to be applied to each pair of observations.  The default
is equal weights for each pair.  Normalization takes place within the function
so \code{sum(w)} need not equal 1.
}}
\value{
The correlation coefficient between \code{d[,1]} and \code{d[,2]}.
}
\details{
This function finds the correlation coefficient in weighted form.  This is
often useful in bootstrap methods since it allows for numerical differentiation
to get the empirical influence values.  It is also necessary to have the 
statistic in this form to find ABC intervals.
}
\seealso{
\code{\link{cor}}
}
\keyword{math}
\keyword{multivariate}
% Converted by Sd2Rd version 1.15.
