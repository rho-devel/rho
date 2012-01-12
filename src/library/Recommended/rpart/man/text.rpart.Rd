\name{text.rpart}
\alias{text.rpart}
\title{
  Place Text on a Dendrogram 
}
\description{
  Labels the current plot of the tree dendrogram with text.
}
\usage{
\method{text}{rpart}(x, splits=TRUE, label, FUN=text, all=FALSE,
     pretty=NULL, digits=getOption("digits") - 3, use.n=FALSE,
     fancy=FALSE, fwidth=0.8, fheight=0.8, \dots)
}
\arguments{
\item{x}{
  fitted model object of class \code{rpart}.  This is assumed to be the result
  of some function that produces an object with the same named
  components as that returned by the \code{rpart} function.
}
\item{splits}{
  logical flag.  If \code{TRUE} (default), then the splits in the tree are
  labeled with the criterion for the split.
}
\item{label}{
% a column name of \code{x$frame};  values of this
% will label the nodes.  For the \code{"class"} method,
% \code{label="yval"} results in the factor levels
% being used, \code{"yprob"} results in the probability of the winning factor
% level being used, and \sQuote{specific yval level} results in the probability
% of that factor level.
For compatibility with \code{rpart2}, ignored in this version (with
a warning).
}
\item{FUN}{
  the name of a labeling function, e.g. \code{text}.  
}
\item{all}{
  Logical.  If \code{TRUE}, all nodes are labeled, otherwise just
  terminal nodes.
}
\item{pretty}{
  an integer denoting the extent to which factor levels in split labels
  will be abbreviated.  A value of (0) signifies no abbreviation.  A
  \code{NULL}, the default, signifies using elements of letters to represent
  the different factor levels.
}
\item{digits}{
  number of significant digits to include in numerical labels.
}
\item{use.n}{
  Logical.  If \code{TRUE}, adds to label 
  (\#events level1/ \#events level2/etc. for \code{class},
  \code{n} for \code{anova}, and \#events/n for \code{poisson} and \code{exp}).
}
\item{fancy}{
  Logical.  If \code{TRUE}, nodes are represented by ellipses (interior nodes)
  and rectangles (leaves) and labeled by \code{yval}.  The edges connecting the
  nodes are labeled by left and right splits.
}
\item{fwidth}{
  Relates to option \code{fancy} and the width of the ellipses and rectangles.
  If \code{fwidth < 1} then it is a scaling factor (default = 0.8).  If 
  \code{fwidth > 1} then it represents the number of character widths 
  (for current graphical device) to use.
}
\item{fheight}{
  Relates to option \code{fancy} and the height of the ellipses and rectangles.
  If \code{fheight <1} then it is a scaling factor (default = 0.8).  If
  \code{fheight > 1} then it represents the number of character heights 
  (for current graphical device) to use.
}
\item{\dots}{
  Graphical parameters may also be supplied as arguments to this
  function (see \code{par}).  As labels often extend outside the plot
  region it can be helpful to specify \code{xpd = TRUE}.
}}
\section{Side Effects}{
  the current plot of a tree dendrogram is labeled.
}
\seealso{
  \code{\link{text}}, \code{\link{plot.rpart}}, \code{\link{rpart}},
  \code{\link{post.rpart}}, \code{\link{abbreviate}} 
}
\examples{
freen.tr <- rpart(y ~ ., freeny)
plot(freen.tr)
text(freen.tr, use.n=TRUE, all=TRUE)
}
\keyword{tree}
