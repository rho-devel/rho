\name{dMatrix-class}
\docType{class}
\alias{dMatrix-class}
\alias{lMatrix-class}
%
\alias{show,dMatrix-method}
%\alias{coerce,dMatrix,matrix-method}
\alias{coerce,dMatrix,lMatrix-method}
\alias{coerce,lMatrix,dMatrix-method}
\alias{coerce,lMatrix,dgCMatrix-method}
\alias{coerce,matrix,lMatrix-method}
%\alias{coerce,dMatrix,dgeMatrix-method}
\alias{[,dMatrix,lMatrix,missing,ANY-method}
\alias{[,dMatrix,logical,missing,ANY-method}
% Group methods
\alias{Ops,dMatrix,dMatrix-method}
\alias{Ops,dMatrix,lMatrix-method}
\alias{Ops,dMatrix,nMatrix-method}
\alias{Ops,lMatrix,dMatrix-method}
\alias{Ops,lMatrix,lMatrix-method}
\alias{Ops,lMatrix,numeric-method}
\alias{Ops,nMatrix,dMatrix-method}
\alias{Ops,numeric,lMatrix-method}
\alias{Arith,dMatrix,dMatrix-method}
\alias{Arith,lMatrix,logical-method}
\alias{Arith,lMatrix,numeric-method}
\alias{Arith,logical,lMatrix-method}
\alias{Arith,numeric,lMatrix-method}
\alias{Compare,dMatrix,logical-method}
\alias{Compare,dMatrix,numeric-method}
\alias{Compare,lMatrix,logical-method}
\alias{Compare,lMatrix,numeric-method}
\alias{Compare,logical,dMatrix-method}
\alias{Compare,logical,lMatrix-method}
\alias{Compare,numeric,dMatrix-method}
\alias{Compare,numeric,lMatrix-method}
\alias{Logic,dMatrix,logical-method}
\alias{Logic,dMatrix,numeric-method}
\alias{Logic,lMatrix,logical-method}
\alias{Logic,lMatrix,numeric-method}
\alias{Logic,logical,dMatrix-method}
\alias{Logic,logical,lMatrix-method}
\alias{Logic,numeric,dMatrix-method}
\alias{Logic,numeric,lMatrix-method}
\alias{Summary,lMatrix-method}
%% R <= 2.5.x :
% \alias{Math2,dMatrix,ANY-method}% Math2 = round + signif,.., but
% \alias{Math2,dMatrix,missing-method}
% % for silly reasons, need these 2+3 as well:
% \alias{round,dMatrix,numeric-method}
% \alias{signif,dMatrix,numeric-method}
%% R-2.6.0 - maybe
\alias{Math2,dMatrix-method}
\alias{log,dMatrix-method}
\alias{gamma,dMatrix-method}
\alias{lgamma,dMatrix-method}
%
\alias{zapsmall,dMatrix-method}
\alias{\%*\%,nMatrix,lMatrix-method}
\alias{\%*\%,lMatrix,nMatrix-method}
\alias{\%*\%,lMatrix,lMatrix-method}
\alias{\%*\%,nMatrix,nMatrix-method}
\alias{\%*\%,dMatrix,lMatrix-method}
\alias{\%*\%,dMatrix,nMatrix-method}
\alias{\%*\%,lMatrix,dMatrix-method}
\alias{\%*\%,nMatrix,dMatrix-method}
%\alias{which,lMatrix-method}
%
% operations with one argument from the "integer" class
\alias{crossprod,dMatrix,integer-method}
\alias{crossprod,integer,dMatrix-method}
%\alias{solve,dMatrix,integer-method}--> solve-methods.Rd
\alias{\%*\%,dMatrix,integer-method}
\alias{\%*\%,integer,dMatrix-method}
\title{(Virtual) Class "dMatrix" of "double" Matrices}
\description{
  The \code{dMatrix} class is a virtual class contained by all actual
  classes of numeric matrices in the \pkg{Matrix} package.  Similarly,
  all the actual classes of logical matrices inherit from the
  \code{lMatrix} class.
}
%\section{Objects from the Class}{A virtual Class: No objects may be
%  created from it.
%}
\section{Slots}{
  Common to \emph{all} matrix object in the package:
  \describe{
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
     of the matrix - must be an integer vector with exactly two
     non-negative values.}
    \item{\code{Dimnames}:}{list of length two; each component
      containing NULL or a \code{\link{character}} vector length
      equal the corresponding \code{Dim} element.}
  }
}
\section{Methods}{
  There are (relatively simple) group methods (see, e.g., \code{\link{Arith}})
  \describe{
    \item{Arith}{\code{signature(e1 = "dMatrix", e2 = "dMatrix")}: ... }
    \item{Arith}{\code{signature(e1 = "dMatrix", e2 = "numeric")}: ... }
    \item{Arith}{\code{signature(e1 = "numeric", e2 = "dMatrix")}: ... }
    \item{Math}{\code{signature(x = "dMatrix")}: ... }
    \item{Math2}{\code{signature(x = "dMatrix", digits = "numeric")}:
      this group contains \code{\link{round}()} and \code{\link{signif}()}.}
    \item{Compare}{\code{signature(e1 = "numeric", e2 = "dMatrix")}: ... }
    \item{Compare}{\code{signature(e1 = "dMatrix", e2 = "numeric")}: ... }
    \item{Compare}{\code{signature(e1 = "dMatrix", e2 = "dMatrix")}: ... }
    \item{Summary}{\code{signature(x = "dMatrix")}: The \code{"Summary"}
      group contains the seven functions
      \code{\link{max}()}, \code{\link{min}()}, \code{\link{range}()},
      \code{\link{prod}()}, \code{\link{sum}()},
      \code{\link{any}()}, and \code{\link{all}()}.}
  }
  The following methods are also defined for all double matrices:
  \describe{
    \item{coerce}{\code{signature(from = "dMatrix", to = "matrix")}: ... }
    %
    \item{expm}{\code{signature(x = "dMatrix")}: computes the
      \emph{\dQuote{Matrix Exponential}}, see \code{\link{expm}}.}
    \item{zapsmall}{\code{signature(x = "dMatrix")}: ... }
  }
  The following methods are defined for all logical matrices:
  \describe{
    \item{which}{\code{signature(x = "lsparseMatrix")} and many other
      subclasses of \code{"lMatrix"}: as the \pkg{base} function
      \code{\link{which}(x, arr.ind)} returns the indices of the
      \code{\link{TRUE}} entries in \code{x}; if \code{arr.ind} is true,
      as a 2-column matrix of row and column indices.}
  }
}
%\references{}
% Martin + Doug\author{Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  The nonzero-pattern matrix class \code{\linkS4class{nMatrix}}, which
  can be used to store non-\code{\link{NA}} \code{\link{logical}}
  matrices even more compactly.

  The numeric matrix classes \code{\linkS4class{dgeMatrix}},
  \code{\linkS4class{dgCMatrix}}, and \code{\linkS4class{Matrix}}.

  \code{\link{drop0}(x, tol=1e-10)} is sometimes preferable to (and
  more efficient than) \code{zapsmall(x, digits=10)}.
}
\examples{
 showClass("dMatrix")

 set.seed(101)
 round(Matrix(rnorm(28), 4,7), 2)
 M <- Matrix(rlnorm(56, sd=10), 4,14)
 (M. <- zapsmall(M))
 table(as.logical(M. == 0))
}
\keyword{classes}
\keyword{algebra}
