\name{initial.sp}
\alias{initial.sp}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Starting values for multiple smoothing parameter estimation}
\description{ Finds initial smoothing parameter guesses for multiple smoothing
parameter estimation. The idea is to find values such that the estimated
degrees of freedom per penalized parameter should be well away from 0 and 1
for each penalized parameter, thus ensuring that the values are in a region of
parameter space where the smoothing parameter estimation criterion is varying
substantially with smoothing parameter value. 
} %- end description

\usage{
initial.sp(X,S,off,expensive=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{

\item{X}{is the model matrix.}

\item{S}{ is a list of of penalty matrices. \code{S[[i]]} is the ith penalty matrix, but note
that it is not stored as a full matrix, but rather as the smallest square matrix including all 
the non-zero elements of the penalty matrix. Element 1,1 of \code{S[[i]]}  occupies 
element \code{off[i]}, \code{off[i]} of the ith penalty matrix. Each \code{S[[i]]} must be 
positive semi-definite.  }

\item{off}{is an array indicating the first parameter in the parameter vector that is 
penalized by the penalty involving \code{S[[i]]}.}

\item{expensive}{if \code{TRUE} then the overall amount of smoothing is
adjusted so that the average degrees of freedom per penalized parameter is
exactly 0.5: this is numerically costly. }
}

\details{ Basically uses a crude approximation to the estimated degrees of
freedom per model coefficient, to try and find smoothing parameters which
bound these e.d.f.'s away from 0 and 1.

Usually only called by \code{\link{magic}} and \code{\link{gam}}.
}

\value{ An array of initial smoothing parameter estimates.
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{  
\code{\link{magic}},
\code{\link{gam.outer}},
\code{\link{gam}},
}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..





