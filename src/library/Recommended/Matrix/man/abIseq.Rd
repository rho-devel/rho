\name{abIseq}
\title{Sequence Generation of "abIndex", Abstract Index Vectors}
\Rdversion{1.1}
%
\alias{abIseq}
\alias{abIseq1}
\alias{c.abIndex}
%
\description{
  Generation of abstract index vectors, i.e., objects of class
  \code{"\linkS4class{abIndex}"}.

  \code{abIseq()} is designed to work entirely like \code{\link{seq}},
  but producing \code{"abIndex"} vectors.\cr
  \code{abIseq1()} is its basic building block, where
  \code{abIseq1(n,m)} corresponds to \code{n:m}.

  \code{c(x, ...)} will return an \code{"abIndex"} vector, when \code{x}
  is one.
}
\usage{
abIseq1(from = 1, to = 1)
abIseq (from = 1, to = 1, by = ((to - from)/(length.out - 1)),
        length.out = NULL, along.with = NULL)

\method{c}{abIndex}(\dots)
}
\arguments{
  \item{from, to}{the starting and (maximal) end value of the sequence.}
  \item{by}{number: increment of the sequence.}
  \item{length.out}{desired length of the sequence.  A
    non-negative number, which for \code{seq} and \code{seq.int} will be
    rounded up if fractional.}
  \item{along.with}{take the length from the length of this argument.}

  \item{\dots}{in general an arbitrary number of \R objects; here,
    when the first is an \code{"\linkS4class{abIndex}"} vector, these
    arguments will be concatenated to a new \code{"abIndex"} object.}
}
%\author{Martin Maechler}
% \details{

% }
\value{
  An abstract index vector, i.e., object of class
  \code{"\linkS4class{abIndex}"}.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\seealso{
  the class \code{\linkS4class{abIndex}} documentation;
  \code{\link{rep2abI}()} for another constructor;
  \code{\link{rle}} (\pkg{base}).
}
\examples{
stopifnot(identical(-3:20,
                    as(abIseq1(-3,20), "vector")))

try( ## (arithmetic) not yet implemented
abIseq(1, 50, by = 3)
)

%% FIXME: add / exchange with ../tests/abIndex-tsts.R
}
\keyword{manip}
\keyword{classes}
