\name{freq.array}
\alias{freq.array}
\title{
Bootstrap Frequency Arrays
}
\description{
Take a matrix of indices for nonparametric bootstrap resamples and
return the frequencies of the original observations in each resample.
}
\usage{
freq.array(i.array)
}
\arguments{
\item{i.array}{
This will be an matrix of integers between 1 and n, where n is the number
of observations in a data set.  The matrix will have n columns and R rows
where R is the number of bootstrap resamples.  Such matrices are found by
\code{boot} when doing nonparametric bootstraps.  They can also be found after a
bootstrap has been run through the function \code{boot.array}.
}}
\value{
A matrix of the same dimensions as the input matrix.  Each row of the 
matrix corresponds to a single bootstrap resample.  Each column of the
matrix corresponds to one of the original observations and specifies its
frequency in each bootstrap resample.  Thus the first column tells us how
often the first observation appeared in each bootstrap resample.  Such
frequency arrays are often useful for diagnostic purposes such as the
jackknife-after-bootstrap plot.  They are also necessary for the regression
estimates of empirical influence values and for finding importance sampling 
weights.
}
\seealso{
\code{\link{boot.array}}
}
\keyword{nonparametric}
% Converted by Sd2Rd version 1.15.
