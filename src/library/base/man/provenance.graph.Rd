% File src/library/base/man/provenance.graph.Rd
% Part of CXXR, http://www.cs.kent.ac.uk/projects/cxxr
% Copyright 2012-3 Andrew R Runnalls
% Distributed under GPL 2 or later

\name{provenance.graph}
\title{Determine Provenance Graph}
\alias{provenance.graph}
\description{
  \code{provenance.graph()} determines the provenance graph of bindings within
  the global environment.
}
\usage{
provenance.graph(names)
}
\arguments{
  \item{names}{a character vector listing the names of zero or more symbols
    currently bound within the global environment.}
}
\details{
  This is a CXXR function enabled only if CXXR has been configured with
  provenance tracking enabled.

  It returns details of the provenance graph of the current bindings of
  the specified symbols.  This is the graph comprising the specified
  bindings and all their ancestor bindings (to the extent that
  provenance information has been recorded).
}
\value{
  \code{provenance.graph()} returns a list of seven components.  The
  first three components relate to the nodes (bindings) within the
  provenance graph, and are each equal in length to the number of nodes
  in the graph.  The nodes are arranged in a topological ordering, with
  parents preceding children.

  \item{symbols}{A list of symbols.  \code{symbols[[i]]} is the
  symbol bound by the binding forming the \code{i}th node of the
  provenance graph.} 

  \item{commands}{A list of R objects, normally expressions.
  \code{commands[[i]]} is the top-level command that gave rise to the
  binding forming the \code{i}th node of the provenance graph.}

  \item{timestamps}{A vector of timestamps of the POSIXct class, in
    ascending order.  \code{timestamps[i]} is the time at which the
    top-level command \code{commands[[i]]} was issued.}

  The next two components of the return value relate to xenogenous
  bindings.  Normally the value of a binding is completely determined as
  a function \emph{(a)} of the top-level command that created it, and
  \emph{(b)} of the bindings read in carrying out that command.
  Sometimes however this functional determination does not hold, for
  example in the case of commands that read from a file or invoke user
  interaction; in this case the resulting bindings are described a
  xenogenous (\sQuote{coming from outside}).  For xenogenous bindings
  (only), the provenance-tracking system records the values to which
  symbols were bound, to facilitate reproducibility.

  \item{xenogenous}{An integer vector, possibly empty, containing in
  ascending order the index numbers (counting from 1) of the graph nodes
  representing a xenogenous binding.  For example, if
  \code{xenogenous} contains the value \code{i}, this signifies that the
  binding of \code{symbols[[i]]} created by \code{commands[[i]]} was
  xenogenous.}

  \item{values}{A list of the same length as \code{xenogenous},
  containing the values bound by the xenogenous bindings.  For example,
  if \code{xenogenous[3]} is equal to 5, then \code{values[[3]]}
  contains the value to which \code{symbols[[5]]} was bound as a result
  of the top-level command \code{commands[[5]]}.}

  The final two components of the return value define the directed edges
  of the provenance graph.  The edges are sorted with the parent index
  being the major sort key, and the child index the minor sort key.

  \item{parents}{an integer vector giving, for each edge, the index
  number of the parent node of that edge.}

  \item{children}{an integer vector of the same length as \code{parents}
  giving, for each edge, the index number of the child node of that edge.}
}
\examples{
\dontrun{
x <- 1:10
x2 <- x + 2
sq <- function(y){y*y}
z <- sq(x)
provenance.graph("z")
provenance.graph(ls())
}
}
