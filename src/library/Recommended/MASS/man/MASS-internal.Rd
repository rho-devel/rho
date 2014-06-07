\name{MASS-internal}
\alias{enlist}
\alias{fbeta}
\alias{frequency.polygon}
\alias{nclass.freq}
\alias{neg.bin}
\alias{negexp.SSival}
%\alias{pairs.profile}
%\alias{plot.profile}
%\alias{print.Anova}
%\alias{print.abbrev}
\title{Internal MASS functions}
\description{
 Internal MASS functions.
}
\usage{
enlist(vec)
fbeta(x, alpha, beta)
frequency.polygon(x, nclass = nclass.freq(x), xlab="", ylab="", \dots)
nclass.freq(x)
neg.bin(theta = stop("'theta' must be given"))
negexp.SSival(mCall, data, LHS)
%pairs.profile(x, colours = 2:3, \dots)
%plot.profile(x, nseg, \dots)
%print.Anova(x, \dots)
}
\details{
  These are not intended to be called by the user.
  Some are for compatibility
  with earlier versions of MASS (the book).
}
\keyword{internal}
