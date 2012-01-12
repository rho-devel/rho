\name{ruspini}
\alias{ruspini}
\title{Ruspini Data}
\usage{data(ruspini)}
\description{
  The Ruspini data set, consisting of 75 points in four groups that is
  popular for illustrating clustering techniques.
}
\format{
  A data frame with 75 observations on 2 variables giving the x and y
  coordinates of the points, respectively.
}
\source{
  E. H. Ruspini (1970):
  Numerical methods for fuzzy clustering.
  \emph{Inform. Sci.}, \bold{2}, 319--350.
}
\references{
  Anja Struyf, Mia Hubert & Peter J. Rousseeuw (1996):
  Clustering in an Object-Oriented Environment.
  \emph{Journal of Statistical Software}, \bold{1}.
  \url{http://www.stat.ucla.edu/journals/jss/}
}
\examples{
data(ruspini)

## Plot similar to Figure 4 in Stryuf et al (1996)
\dontrun{plot(pam(ruspini, 4), ask = TRUE)}
\testonly{plot(pam(ruspini, 4))}

## Plot similar to Figure 6 in Stryuf et al (1996)
plot(fanny(ruspini, 5))
}
\keyword{datasets}
