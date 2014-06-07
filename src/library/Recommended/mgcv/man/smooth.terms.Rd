\name{smooth.terms}
\alias{smooth.terms}
\title{Smooth terms in GAM}
\description{
Smooth terms are specified in a \code{\link{gam}} formula using \code{\link{s}}, \code{\link{te}}, \code{\link{ti}} 
and \code{\link{t2}} terms. 
Various smooth classes are available, for different modelling tasks, and users can add smooth classes 
(see \code{\link{user.defined.smooth}}). What defines a smooth class is the basis used to represent 
the smooth function and quadratic penalty (or multiple  penalties) used to penalize 
the basis coefficients in order to control the degree of smoothness. Smooth classes are 
invoked directly by \code{s} terms, or as building blocks for tensor product smoothing 
via \code{te}, \code{ti} or \code{t2} terms (only smooth classes with single penalties can be used in tensor products). The smooths
built into the \code{mgcv} package are all based one way or another on low rank versions of splines. For the full rank 
versions see Wahba (1990).

Note that smooths can be used rather flexibly in \code{gam} models. In particular the linear predictor of the GAM can 
depend on (a discrete approximation to) any linear functional of a smooth term, using \code{by} variables and the 
`summation convention' explained in \code{\link{linear.functional.terms}}.

The single penalty built in smooth classes are summarized as follows
\describe{
\item{Thin plate regression splines}{\code{bs="tp"}. These are low rank isotropic smoothers of any number of covariates. By isotropic is 
meant that rotation of the covariate co-ordinate system will not change the result of smoothing. By low rank is meant 
that they have far fewer coefficients than there are data to smooth. They are reduced rank versions of the thin plate splines and use the thin plate spline penalty. They are the default
smooth for \code{s} terms because there is a defined sense in which they are the optimal smoother of any given
basis dimension/rank (Wood, 2003). Thin plate regression splines do not have `knots' 
(at least not in any conventional sense): a truncated eigen-decomposition is used to achieve the rank reduction.  See \code{\link{tprs}} for further details. 

\code{bs="ts"} is as \code{"tp"} but with a modification to the smoothing penalty, so that the null space is also penalized slightly and the 
whole term can therefore be shrunk to zero.}

\item{Duchon splines}{\code{bs="ds"}. These generalize thin plate splines. In particular, for any given number of covariates they 
allow lower orders of derivative in the penalty than thin plate splines (and hence a smaller null space).
See \code{\link{Duchon.spline}} for further details. 
}

\item{Cubic regression splines}{\code{bs="cr"}. 
These have a cubic spline basis defined by a modest sized 
set of knots spread evenly through the 
covariate values. They are penalized by the conventional intergrated square second derivative cubic spline penalty. 
For details see \code{\link{cubic.regression.spline}} and e.g. Wood (2006a).

\code{bs="cs"} specifies a shrinkage version of \code{"cr"}.

\code{bs="cc"} specifies a cyclic cubic regression splines (see \link{cyclic.cubic.spline}). i.e. a penalized cubic regression splines whose ends match, up to second 
derivative.}

\item{Splines on the sphere}{\code{bs="sos"}.
These are two dimensional splines on a sphere. Arguments are latitude and longitude, and they are 
the analogue of thin plate splines for the sphere. Useful for data sampled over a large portion of the globe, 
when isotropy is appropriate. See \code{\link{Spherical.Spline}} for details.}

\item{P-splines}{\code{bs="ps"}. 
These are P-splines as proposed by Eilers and Marx (1996). They combine a B-spline basis, with a discrete penalty
on the basis coefficients, and any sane combination of penalty and basis order is allowed. Although this penalty has no exact interpretation in terms of function shape, in the way that the derivative penalties do, P-splines perform almost as well as conventional splines in many standard applications, and can perform better in particular cases where it is advantageous to mix different orders of basis and penalty.

\code{bs="cp"} gives a cyclic version of a P-spline (see \link{cyclic.p.spline}). }

\item{Random effects}{\code{bs="re"}. These are parametric terms penalized by a ridge penalty (i.e. the identity matrix). When such a smooth has multiple arguments 
then it represents the parametric interaction of these arguments, with the coefficients penalized by a ridge penalty. The ridge penalty is equivalent to an 
assumption that the coefficients are i.i.d. normal random effects. See \code{\link{smooth.construct.re.smooth.spec}}.}

\item{Markov Random Fields}{\code{bs="mrf"}. These are popular when space is split up into discrete contiguous 
geographic units (districts of a town, for example). In this case a simple smoothing penalty is constructed
based on the neighbourhood structure of the geographic units. See \code{\link{mrf}} for details and an example.}

\item{Soap film smooths}{\code{bs="so"} (actually not single penaltied, but \code{bs="sw"} and \code{bs="sf"} allows splitting into single penalty components for use in tensor product smoothing). These are finite area smoothers designed to smooth within complicated geographical boundaries, where the boundary matters (e.g. you do not want to smooth across boundary features). See \code{\link{soap}} for details.}

}

Broadly speaking the default penalized thin plate regression splines tend to give the best MSE performance, 
but they are slower to set up than the other bases. The knot based penalized cubic regression splines
(with derivative based penalties) usually come next in MSE performance, with the P-splines doing 
just a little worse. However the P-splines are useful in non-standard situations.

All the preceding classes (and any user defined smooths with single penalties) may be used as marginal 
bases for tensor product smooths specified via \code{\link{te}}, \code{\link{ti}} or \code{\link{t2}} terms. Tensor 
product smooths are smooth functions 
of several variables where the basis is built up from tensor products of bases for smooths of fewer (usually one) 
variable(s) (marginal bases). The multiple penalties for these smooths are produced automatically from the
penalties of the marginal smooths. Wood (2006b) and Wood, Scheipl and Faraway (2012), give the general recipe for these constructions. The \code{te} construction results in fewer, more interpretable, penalties, while the \code{t2} construction is more natural if you are interested in functional ANOVA decompositions. \code{t2} works with the 
\code{gamm4} package. 

Tensor product smooths often perform better than isotropic smooths when the covariates of a smooth are not naturally
on the same scale, so that their relative scaling is arbitrary. For example, if smoothing with repect to time and 
distance, an isotropic smoother will give very different results if the units are cm and minutes compared to if the units are metres and seconds: a tensor product smooth will  give the same answer in both cases (see \code{\link{te}} for an example of this). Note that \code{te} terms are knot based, and the thin plate splines seem to offer no advantage over cubic or P-splines as  marginal bases.
 
Some further specialist smoothers that are not suitable for use in tensor products are also available.
\describe{
\item{Adaptive smoothers}{\code{bs="ad"}
Univariate and bivariate adaptive smooths are available (see \code{\link{adaptive.smooth}}). 
These are appropriate when the degree of smoothing should itself vary with the covariates to be smoothed, and the 
data contain sufficient information to be able to estimate the appropriate variation. Because this flexibility is 
achieved by splitting the penalty into several `basis penalties' these terms are not suitable as components of tensor 
product smooths, and are not supported by \code{gamm}.}

\item{Factor smooth interactions}{\code{bs="fs"}
Smooth factor interactions are often produced using \code{by} variables (see \code{\link{gam.models}}), but a special smoother
class (see \code{\link{factor.smooth.interaction}}) is available for the case in which a smooth is required at each of a large number of factor levels (for example a smooth for each patient in a study), and each smooth should have the same smoothing parameter. The \code{"fs"} smoothers are set up to be efficient when used with \code{\link{gamm}}, and have penalties on each null sapce component (i.e. they are fully `random effects'). 
}
}

}

\seealso{\code{\link{s}}, \code{\link{te}}, \code{\link{t2}} \code{\link{tprs}},\code{\link{Duchon.spline}}, 
\code{\link{cubic.regression.spline}},\code{\link{p.spline}}, \code{\link{mrf}}, code{\link{soap}}, 
code{\link{Spherical.Spline}}, \code{\link{adaptive.smooth}}, \code{\link{user.defined.smooth}},
code{\link{smooth.construct.re.smooth.spec}}, code{\link{factor.smooth.interaction}}}

\author{
Simon Wood <simon.wood@r-project.org>
}
\references{
Eilers, P.H.C. and B.D. Marx (1996) Flexible Smoothing with B-splines and Penalties. 
Statistical Science, 11(2):89-121

Wahba (1990) Spline Models of Observational Data. SIAM 

Wood, S.N. (2003) Thin plate regression splines. J.R.Statist.Soc.B 65(1):95-114

Wood, S.N. (2006a) \emph{Generalized Additive Models: an introduction with R}, CRC

Wood, S.N. (2006b) Low rank scale invariant tensor product smooths for
generalized additive mixed models. Biometrics 62(4):1025-1036

Wood S.N., F. Scheipl and J.J. Faraway (2012) Straightforward intermediate rank tensor product smoothing
in mixed models. Statistical Computing.

}

\examples{
## see examples for gam and gamm
}
\keyword{regression}