\name{mona}
\alias{mona}
\title{MONothetic Analysis Clustering of Binary Variables}

\description{
  Returns a list representing a divisive hierarchical clustering of
  a dataset with binary variables only.
}
\usage{
mona(x)
}
\arguments{
  \item{x}{
    data matrix or data frame in which each row corresponds to an
    observation, and each column corresponds to a variable.  All
    variables must be binary.  A limited number of missing values (NAs)
    is allowed. Every observation must have at least one value different
    from NA.  No variable should have half of its values missing. There
    must be at least one variable which has no missing values.  A
    variable with all its non-missing values identical, is not allowed.
  }
}
\value{
  an object of class \code{"mona"} representing the clustering.
  See \code{mona.object} for details.
}
\details{
\code{mona} is fully described in chapter 7 of Kaufman and Rousseeuw (1990).
It is "monothetic" in the sense that each division is based on a
single (well-chosen) variable, whereas most other hierarchical methods
(including \code{agnes} and \code{diana}) are "polythetic", i.e. they use
all variables together.

The \code{mona}-algorithm constructs a hierarchy of clusterings,
starting with one large
cluster. Clusters are divided until all observations in the same cluster have
identical values for all variables.\cr
At each stage, all clusters are divided according to the values of one
variable. A cluster is divided into one cluster with all observations having
value 1 for that variable, and another cluster with all observations having
value 0 for that variable.

The variable used for splitting a cluster is the variable with the maximal
total association to the other variables, according to the observations in the
cluster to be splitted. The association between variables f and g
is given by a(f,g)*d(f,g) - b(f,g)*c(f,g), where a(f,g), b(f,g), c(f,g),
and d(f,g) are the numbers in the contingency table of f and g.
[That is, a(f,g) (resp. d(f,g)) is the number of observations for which f and g
both have value 0 (resp. value 1); b(f,g) (resp. c(f,g)) is the number of
observations for which f has value 0 (resp. 1) and g has value 1 (resp. 0).]
The total association of a variable f is the sum of its associations to all
variables.

This algorithm does not work with missing values, therefore the data are
revised, e.g. all missing values are filled in. To do this, the same measure
of association between variables is used as in the algorithm. When variable
f has missing values, the variable g with the largest absolute association
to f is looked up. When the association between f and g is positive,
any missing value of f is replaced by the value of g for the same
observation. If the association between f and g is negative, then any missing
value of f is replaced by the value of 1-g for the same
observation.
}
\seealso{
  \code{\link{agnes}} for background and references;
  \code{\link{mona.object}}, \code{\link{plot.mona}}.
}
\examples{
data(animals)
ma <- mona(animals)
ma
## Plot similar to Figure 10 in Struyf et al (1996)
plot(ma)
}
\keyword{cluster}

