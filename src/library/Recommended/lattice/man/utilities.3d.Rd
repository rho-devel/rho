\name{G_utilities.3d}
\alias{utilities.3d}
\alias{palette.shade}
\alias{ltransform3dMatrix}
\alias{ltransform3dto3d}
\title{ Utility functions for 3-D plots }
\description{
  These are (related to) the default panel functions for \code{cloud}
  and \code{wireframe}.
}
\usage{
ltransform3dMatrix(screen, R.mat)
ltransform3dto3d(x, R.mat, dist)
% palette.shade(irr, ref, height, saturation)
}
\arguments{
  \item{x}{
    \code{x} can be a numeric matrix with 3 rows for
    \code{ltransform3dto3d}
  }
  \item{screen}{list, as described in \code{\link{panel.cloud}}}
  \item{R.mat}{4x4 transformation matrix in homogeneous
    coordinates}
  \item{dist}{controls transformation to account for
    perspective viewing}

%   \item{irr, ref}{
%     numeric, between 0 and 1. See details below. (The names are intended
%     to suggest a link with irradiance and reflectance respectively.)
%   }
%   \item{height}{
%     numeric, denoting height scaled to lie between 0 and 1
%   }
%   \item{saturation}{
%     used to control \code{s} in \code{hsv} inside \code{palette.shade}
%   }
}
\details{
  \code{ltransform3dMatrix} and \code{ltransform3dto3d} are utility
  functions to help in computation of projections. These functions are
  used inside the panel functions for \code{cloud} and
  \code{wireframe}. They may be useful in user-defined panel functions
  as well.

  The first function takes a list of the form of the \code{screen}
  argument in \code{cloud} and \code{wireframe} and a \code{R.mat}, a
  4x4 transformation matrix in homogeneous coordinates, to return a new
  4x4 transformation matrix that is the result of applying \code{R.mat}
  followed by the rotations in \code{screen}. The second function
  applies a 4x4 transformation matrix in homogeneous coordinates to a
  3xn matrix representing points in 3-D space, and optionally does some
  perspective computations. (There has been no testing with non-trivial
  transformation matrices, and my knowledge of the homogeneous
  coordinate system is very limited, so there may be bugs here.)

%   \code{palette.shade} is the function used by default for calculation
%   of colors when \sQuote{shading} a wireframe. Such a function can expect three
%   arguments in general, namely \code{irr}, \code{ref} and
%   \code{height}.  \code{irr} should represent foreshortening, being the
%   cosine of the angle between the incident light ray and the normal to
%   the surface.  \code{ref} represents the direction of the reflected
%   light, and in fact is the cosine of half the angle between the
%   reflected ray and the viewing direction. \code{height} is the scaled
%   (average) height (of any particular facet).

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{cloud}}, \code{\link{panel.cloud}}}
\keyword{dplot}
