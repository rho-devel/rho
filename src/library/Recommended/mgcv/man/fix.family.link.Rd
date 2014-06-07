\name{fix.family.link}
\alias{fix.family.link}
\alias{fix.family.var}
\alias{fix.family.ls}
\alias{fix.family.qf}
\alias{fix.family.rd}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Modify families for use in GAM fitting and checking}
\description{ Generalized Additive Model fitting by `outer' iteration,
requires extra derivatives of the variance and link functions to be 
added to family objects. The first 3 functions add what is needed. Model checking can
be aided by adding quantile and random deviate generating functions to the family. 
The final two functions do this.


}
\usage{
fix.family.link(fam)
fix.family.var(fam)
fix.family.ls(fam)
fix.family.qf(fam)
fix.family.rd(fam)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{fam}{A \code{family}.}
}
\details{ 
Consider the first 3 function first.

Outer iteration GAM estimation requires derivatives of the GCV, UBRE/gAIC,
GACV, REML or ML score, which are obtained by finding the derivatives of the model
coefficients w.r.t. the log smoothing parameters, using the implicit function theorem. 
 The expressions for the derivatives require the second and third derivatives of the link 
w.r.t. the mean (and the 4th derivatives if Fisher scoring is not used). Also required are the first
and second derivatives of the variance function w.r.t. the mean (plus the third 
derivative if Fisher scoring is not used). Finally REML or ML estimation of smoothing parameters
requires the log saturated likelihood and its first two derivatives w.r.t. the scale parameter.
These functions add functions evaluating these quantities to a family. 

If the family already has functions \code{dvar}, \code{d2var}, \code{d3var}, \code{d2link},
\code{d3link}, \code{d4link} and for RE/ML \code{ls}, then these functions simply 
return the family unmodified: this allows non-standard links
to be used with \code{\link{gam}} when using outer iteration (performance
iteration operates with unmodified families). Note that if you only need Fisher scoring then 
\code{d4link} and \code{d3var} can be dummy, as they are ignored. similalry \code{ls} is only needed for 
RE/ML.

The \code{dvar} function is a function of a mean vector, \code{mu}, and returns
a vector of corresponding first derivatives of the family variance
function. The \code{d2link} function is also a function of a vector of mean
values, \code{mu}: it returns a vector of second derivatives of the link,
evaluated at \code{mu}. Higher derivatives are defined similarly.

If modifying your own family, note that you can often get away with supplying
only a \code{dvar} and \code{d2var}, function if your family only requires links that occur in
one of the standard families.

The second two functions are useful for investigating the distribution of residuals and are used by 
\code{\link{qq.gam}}. If possible the functions add quantile (\code{qf}) or random deviate (\code{rd}) generating functions 
to the family. If a family already has \code{qf} or \code{rd} 
functions then it is left unmodified. \code{qf} functions are only available for some families, and for quasi families 
neither type of function is available.

}
\value{A family object with extra component functions \code{dvar},
\code{d2var}, \code{d2link}, \code{d3link}, \code{d4link}, \code{ls}, and possibly \code{qf} and \code{rd}, 
depending on which functions are called.  
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\seealso{  
\code{\link{gam.fit3}}, \code{\link{qq.gam}}} 


\keyword{models} \keyword{regression}%-- one or more ..


