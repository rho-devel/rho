\name{tau}
\alias{tau}
\title{
Tau Particle Decay Modes
}
\description{
The \code{tau} data frame has 60 rows and 2 columns.


The tau particle is a heavy electron-like particle discovered in the 1970's
by Martin Perl at the Stanford Linear Accelerator Center.  Soon after its
production the tau particle decays into various collections of more stable
particles.  About 86\% of the time the decay involves just one charged particle.
This rate has been measured independently 13 times.  


The one-charged-particle event is made up of four major modes of decay as well
as a collection of other events.  The four main types of decay are denoted
rho, pi, e and mu.  These rates have been measured independently 6, 7, 14 and
19 times respectively.  Due to physical constraints each experiment can only
estimate the composite one-charged-particle decay rate or the rate of one of
the major modes of decay.  


Each experiment consists of a major research project involving many years work.
One of the goals of the experiments was to estimate the rate of decay due to 
events other than the four main modes of decay.  These are uncertain events and
so cannot themselves be observed directly.
}
\usage{
tau
}
\format{
This data frame contains the following columns:


\describe{
\item{\code{rate}}{
The decay rate expressed as a percentage.
}
\item{\code{decay}}{
The type of decay measured in the experiment.  It is a factor with levels
\code{1}, \code{rho}, \code{pi}, \code{e} and \code{mu}.
}}}
\source{
The data were obtained from


Efron, B. (1992) Jackknife-after-bootstrap standard errors and influence 
functions (with Discussion). 
\emph{Journal of the Royal Statistical Society, B}, \bold{54}, 83--127.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Hayes, K.G., Perl, M.L. and Efron, B. (1989) Application of the bootstrap
statistical method to the tau-decay-mode problem. 
\emph{Physical Review, D}, \bold{39},
274-279.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
