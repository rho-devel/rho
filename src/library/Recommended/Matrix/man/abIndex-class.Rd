\name{abIndex-class}
\title{Class "abIndex" of Abstract Index Vectors}
\Rdversion{1.1}
\docType{class}
%
\alias{abIndex-class}
%
\alias{seqMat-class}% unexported for now
% hence, we do not yet mention these further below:
\alias{coerce,abIndex,seqMat-method}
\alias{coerce,numeric,seqMat-method}
\alias{coerce,seqMat,abIndex-method}
\alias{coerce,seqMat,numeric-method}
%
\alias{as.integer,abIndex-method}
\alias{as.numeric,abIndex-method}
\alias{[,abIndex,index,ANY,ANY-method}
\alias{coerce,logical,abIndex-method}
\alias{coerce,numeric,abIndex-method}
\alias{coerce,abIndex,integer-method}
\alias{coerce,abIndex,numeric-method}
\alias{coerce,abIndex,vector-method}
\alias{drop,abIndex-method}% not mentioned below {experimental}
\alias{length,abIndex-method}
\alias{show,abIndex-method}
\alias{is.na,abIndex-method}
\alias{is.finite,abIndex-method}% not yet mentioned
\alias{is.infinite,abIndex-method}
%
\alias{Ops,numeric,abIndex-method}
\alias{Ops,abIndex,abIndex-method}
\alias{Ops,abIndex,numeric-method}
\alias{Ops,abIndex,ANY-method}
\alias{Ops,ANY,abIndex-method}
\alias{Arith,abIndex,abIndex-method}
\alias{Arith,abIndex,numLike-method}% \alias{Arith,abIndex,numeric-method}
\alias{Arith,numLike,abIndex-method}% \alias{Arith,numeric,abIndex-method}
%
\alias{Summary,abIndex-method}
%
\description{
  The \code{"abIndex"} \code{\link{class}}, short for \dQuote{Abstract
    Index Vector}, is used for dealing with large index vectors more
  efficiently, than using integer (or \code{\link{numeric}}) vectors of
  the kind \code{2:1000000} or \code{c(0:1e5, 1000:1e6)}.

  Note that the current implementation details are subject to change,
  and if you consider working with these classes, please contact the
  package maintainers (\code{packageDescription("Matrix")$Maintainer}).
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("abIndex", ...)},
  but more easily and typically either by \code{as(x, "abIndex")} where
  \code{x} is an integer (valued) vector, or directly by
  \code{\link{abIseq}()} and combination \code{\link{c}(...)} of such.
}
\section{Slots}{
  \describe{
    \item{\code{kind}:}{a \code{\link{character}} string,
      one of \code{("int32", "double", "rleDiff")}, denoting the
      internal structure of the abIndex object.}
    \item{\code{x}:}{Object of class \code{"numLike"}; is
      used (i.e., not of length \code{0}) only iff the object is \emph{not}
      compressed, i.e., currently exactly when \code{kind != "rleDiff"}.}
    \item{\code{rleD}:}{object of class \code{"\linkS4class{rleDiff}"},
      used for compression via \code{\link{rle}}.}
  }
}
\section{Methods}{
  \describe{
    \item{as.numeric}{\code{signature(x = "abIndex")}: ... }
    \item{[}{\code{signature(x = "abIndex", i = "index", j = "ANY", drop = "ANY")}: ... }
    \item{coerce}{\code{signature(from = "numeric", to = "abIndex")}: ... }
    \item{coerce}{\code{signature(from = "abIndex", to = "numeric")}: ... }
    \item{coerce}{\code{signature(from = "abIndex", to = "integer")}: ... }
    \item{length}{\code{signature(x = "abIndex")}: ... }
    \item{Ops}{\code{signature(e1 = "numeric", e2 = "abIndex")}: These
      and the following arithmetic and logic operations are
      \bold{not yet implemented}; see \code{\link[methods]{Ops}} for a
      list of these (S4) group methods.}
    \item{Ops}{\code{signature(e1 = "abIndex", e2 = "abIndex")}: ... }
    \item{Ops}{\code{signature(e1 = "abIndex", e2 = "numeric")}: ... }
    \item{Summary}{\code{signature(x = "abIndex")}: ... }
    \item{show}{\code{("abIndex")}: simple \code{\link{show}} method,
      building on \code{show(<rleDiff>)}.}
    \item{is.na}{\code{("abIndex")}: works analogously to regular vectors.}
  }
}
\note{
  This is currently experimental and not yet used for our own code.
  Please contact us (\code{packageDescription("Matrix")$Maintainer}),
  if you plan to make use of this class.

  Partly builds on ideas and code from Jens Oehlschlaegel,
  as implemented (around 2008, in the GPL'ed part of) package \pkg{ff}.
}
%\author{Martin Maechler}
\seealso{
  \code{\link{rle}} (\pkg{base}) which is used here;
  \code{\link{numeric}}% ... FIXME
}
\examples{
showClass("abIndex")
ii <- c(-3:40, 20:70)
str(ai <- as(ii, "abIndex"))# note
ai # -> show() method
%% FIXME: add / exchange with ../tests/abIndex-tsts.R

stopifnot(identical(-3:20,
                    as(abIseq1(-3,20), "vector")))
}
\keyword{classes}
