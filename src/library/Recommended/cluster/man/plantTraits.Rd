\name{plantTraits}
\alias{plantTraits}
\title{Plant Species Traits Data}
\docType{data}
\encoding{latin1}
\description{
  This dataset constitutes a description of 136 plant species
  according to biological attributes (morphological or reproductive)
}
\usage{data(plantTraits)
}
\format{
  A data frame with 136 observations on the following 31 variables.
  \describe{
    \item{\code{pdias}}{Diaspore mass (mg)}
    \item{\code{longindex}}{Seed bank longevity}
    \item{\code{durflow}}{Flowering duration}
    \item{\code{height}}{Plant height, an ordered factor with levels
      \code{1} < \code{2} < \dots < \code{8}.}
%     Plant height}{an ordered factor with levels \code{1} <10cm < \code{2} 10-30cm< \code{3} 30-60cm< \code{4}60-100cm < \code{5}1-3m < \code{6}3-6m < \code{7}:6-15m < \code{8}>15m}

    \item{\code{begflow}}{Time of first flowering, an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7} < \code{8} < \code{9}}
%     {\code{begflow}}{an ordered factor with levels \code{1} january< \code{2} february< \code{3} march< \code{4}april < \code{5} may< \code{6} june< \code{7} july< \code{8}august < \code{9}september}

    \item{\code{mycor}}{Mycorrhizas, an ordered factor with levels \code{0}never < \code{1} sometimes< \code{2}always}

    \item{\code{vegaer}}{aerial vegetative propagation, an ordered
      factor with levels \code{0}never < \code{1} present but limited< \code{2}important.}

    \item{\code{vegsout}}{underground vegetative propagation, an ordered
      factor with 3 levels identical to \code{vegaer} above.}

    \item{\code{autopoll}}{selfing pollination, an ordered factor with
      levels \code{0}never < \code{1}rare < \code{2} often< the rule\code{3}}

    \item{\code{insects}}{insect pollination, an ordered factor with 5 levels \code{0} < \dots < \code{4}.}
    \item{\code{wind}}{wind pollination, an ordered factor with 5 levels \code{0} < \dots < \code{4}.}
    \item{\code{lign}}{a binary factor with levels \code{0:1},
      indicating if plant is woody.}
    \item{\code{piq}}{a binary factor indicating if plant is thorny.}
    \item{\code{ros}}{a binary factor indicating if plant is rosette.}
    \item{\code{semiros}}{semi-rosette plant, a binary factor (\code{0}:
      no; \code{1}: yes).}
    \item{\code{leafy}}{leafy plant, a binary factor.}
    \item{\code{suman}}{summer annual, a binary factor.}
    \item{\code{winan}}{winter annual, a binary factor.}
    \item{\code{monocarp}}{monocarpic perennial, a binary factor.}
    \item{\code{polycarp}}{polycarpic perennial, a binary factor.}
    \item{\code{seasaes}}{seasonal aestival leaves, a binary factor.}
    \item{\code{seashiv}}{seasonal hibernal leaves, a binary factor.}
    \item{\code{seasver}}{seasonal vernal leaves, a binary factor.}
    \item{\code{everalw}}{leaves always evergreen, a binary factor.}
    \item{\code{everparti}}{leaves partially evergreen, a binary factor.}
    \item{\code{elaio}}{fruits with an elaiosome (dispersed by ants), a binary factor.}
    \item{\code{endozoo}}{endozoochorous fruits, a binary factor.}
    \item{\code{epizoo}}{epizoochorous fruits, a binary factor.}
    \item{\code{aquat}}{aquatic dispersal fruits, a binary factor.}
    \item{\code{windgl}}{wind dispersed fruits,  a binary factor.}
    \item{\code{unsp}}{unspecialized mechanism of seed dispersal, a binary factor.}
  }
}
\details{
  Most of factor attributes are not disjunctive. For example, a plant can be usually
  pollinated by insects but sometimes self-pollination can occured.
}
\source{
  Vallet, Jeanne (2005)
  \emph{Structuration de communautés végétales et analyse comparative de
  traits biologiques le long d'un gradient d'urbanisation}.
  Mémoire de Master 2 'Ecologie-Biodiversité-Evolution';
  Université Paris Sud XI, 30p.+ annexes (in french)
}
% \references{
%   ~~ possibly secondary sources and usages ~~
% }
\examples{
data(plantTraits)

## Calculation of a dissimilarity matrix
library(cluster)
dai.b <- daisy(plantTraits,
               type = list(ordratio = 4:11, symm = 12:13, asymm = 14:31))

## Hierarchical classification
agn.trts <- agnes(dai.b, method="ward")
plot(agn.trts, which.plots = 2, cex= 0.6)
plot(agn.trts, which.plots = 1)
cutree6 <- cutree(agn.trts, k=6)
cutree6

## Principal Coordinate Analysis
cmdsdai.b <- cmdscale(dai.b, k=6)
plot(cmdsdai.b[, 1:2], asp = 1, col = cutree6)
}
\keyword{datasets}
% plant attribute database, mixed type variables, dissimilarity matrix (DAISY), Hierarchical Classification (AGNES)
% Principal Coordinates Analysis (CMDSCALE)
