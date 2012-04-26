\name{labels.rpart}
\alias{labels.rpart}
\title{
  Create Split Labels For an Rpart Object 
}
\description{
This function provides labels for the branches of an
 \code{rpart} tree.
}
\usage{
\method{labels}{rpart}(object, digits=4, minlength=1L, pretty, collapse=TRUE, ...)
}
\arguments{
\item{object}{
fitted model object of class \code{rpart}.  This is 
assumed to be the result 
of some function that produces an object with the same named 
components as that returned by the \code{rpart} function.
}
\item{digits}{
the number of digits to be used for numeric values.  
All of the \code{rpart} functions that call labels
explicitly set this value, with \code{options("digits")}
as the default.
}
\item{minlength}{
the minimum length for abbreviation of character or factor variables.
If 0 no abbreviation is done; if 1 then single letters are used with
\code{"a"} for the first level, \code{"b"} for the second and so on.
If the value is greater than 1, the 
\code{abbreviate} function is used.
}
\item{pretty}{
an argument included for backwards compatibility:
\code{pretty=0} implies \code{minlength=0},
\code{pretty=NULL} implies \code{minlength=1}, and
\code{pretty=TRUE} implies \code{minlength=4}.
}
\item{collapse}{
logical.
The returned set of labels is always of the same length as the number
of nodes in the tree.  
If \code{collapse=TRUE} (default), 
the returned value is a vector of labels for the branch leading into
each node, with \code{"root"} as the label for the top node.
If \code{FALSE}, the returned value is a two
column matrix of labels for the left and right branches leading 
out from each node, with \code{"leaf"} as the branch labels for terminal
nodes.
}
\item{\dots}{
optional arguments to \code{abbreviate}.
}
}
\value{
Vector of split labels (\code{collapse=TRUE}) or matrix 
of left and right splits 
(\code{collapse=FALSE}) for the supplied 
\code{rpart} object.  This function is called 
by printing methods for \code{rpart} and is not 
intended to be called 
directly by the users. 
}
\seealso{
\code{\link{abbreviate}} 
}
\keyword{tree}
