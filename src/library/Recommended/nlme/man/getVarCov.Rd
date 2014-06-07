% File nlme/man/getVarCov.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{getVarCov}
\alias{getVarCov}
\alias{getVarCov.lme}
\alias{getVarCov.gls}
\alias{print.VarCov}
\title{Extract variance-covariance matrix}
\description{
  Extract the variance-covariance matrix from a fitted model, such as a
  mixed-effects model.
}
\usage{
getVarCov(obj, \dots)
\method{getVarCov}{lme}(obj, individuals,
    type = c("random.effects", "conditional", "marginal"), \dots)
\method{getVarCov}{gls}(obj, individual = 1, \dots)
}
\arguments{
  \item{obj}{A fitted model.  Methods are available for models fit by
    \code{\link{lme}} and by \code{\link{gls}}}
  \item{individuals}{For models fit by \code{\link{lme}} a vector of
    levels of the grouping factor can be specified for the conditional
    or marginal variance-covariance matrices.}
  \item{individual}{For models fit by \code{\link{gls}} the only type of
    variance-covariance matrix provided is the marginal
    variance-covariance of the responses by group.  The
    optional argument \code{individual} specifies the group of responses.}
  \item{type}{For models fit by \code{\link{lme}} the \code{type}
    argument specifies the type of variance-covariance matrix, either
    \code{"random.effects"} for the random-effects variance-covariance
    (the default), or \code{"conditional"} for the conditional.
    variance-covariance of the responses or \code{"marginal"} for the
    the marginal variance-covariance of the responses.}
  \item{\dots}{Optional arguments for some methods, as described above}
}
\value{
  A variance-covariance matrix or a list of variance-covariance matrices.
}
\author{Mary Lindstrom \email{lindstro@biostat.wisc.edu}}
\seealso{\code{\link{lme}}, \code{\link{gls}}}
\examples{
fm1 <- lme(distance ~ age, data = Orthodont, subset = Sex == "Female")
getVarCov(fm1)
getVarCov(fm1, individual = "F01", type = "marginal")
getVarCov(fm1, type = "conditional")
fm2 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
getVarCov(fm2)
}
\keyword{models}
