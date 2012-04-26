% file MASS/man/waders.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{waders}
\alias{waders}
\title{
  Counts of Waders at 15 Sites in South Africa
}
\description{
  The \code{waders} data frame has 15 rows and 19 columns.
  The entries are counts of waders in summer.
}
\usage{
waders
}
\format{
  This data frame contains the following columns (species)
  \describe{
    \item{\code{S1}}{
      Oystercatcher
    }
    \item{\code{S2}}{
      White-fronted Plover
    }
    \item{\code{S3}}{
      Kitt Lutz's Plover
    }
    \item{\code{S4}}{
      Three-banded Plover
    }
    \item{\code{S5}}{
      Grey Plover
    }
    \item{\code{S6}}{
      Ringed Plover
    }
    \item{\code{S7}}{
      Bar-tailed Godwit
    }
    \item{\code{S8}}{
      Whimbrel
    }
    \item{\code{S9}}{
      Marsh Sandpiper
    }
    \item{\code{S10}}{
      Greenshank
    }
    \item{\code{S11}}{
      Common Sandpiper
    }
    \item{\code{S12}}{
      Turnstone
    }
    \item{\code{S13}}{
      Knot
    }
    \item{\code{S14}}{
      Sanderling
    }
    \item{\code{S15}}{
      Little Stint
    }
    \item{\code{S16}}{
      Curlew Sandpiper
    }
    \item{\code{S17}}{
      Ruff
    }
    \item{\code{S18}}{
      Avocet
    }
    \item{\code{S19}}{
      Black-winged Stilt
    }
  }

  The rows are the sites:
  
A = Namibia North coast\cr
B = Namibia North wetland\cr
C = Namibia South coast\cr
D = Namibia South wetland\cr
E = Cape North coast\cr
F = Cape North wetland\cr
G = Cape West coast\cr
H = Cape West wetland\cr
I = Cape South coast\cr
J= Cape South wetland\cr
K = Cape East coast\cr
L = Cape East wetland\cr
M = Transkei coast\cr
N = Natal coast\cr
O = Natal wetland
}
\source{
  J.C. Gower and D.J. Hand (1996) \emph{Biplots}
  Chapman & Hall Table 9.1. Quoted as from:

  R.W. Summers, L.G. Underhill, D.J. Pearson and D.A. Scott (1987)
  Wader migration systems in south and eastern Africa and western Asia.
  \emph{Wader Study Group Bulletin} \bold{49} Supplement, 15--34.
}
\examples{
plot(corresp(waders, nf=2))
}
\keyword{datasets}
