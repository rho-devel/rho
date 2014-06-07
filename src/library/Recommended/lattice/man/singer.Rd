\name{H_singer}
\alias{singer}
\docType{data}
\title{ Heights of New York Choral Society singers }

\description{
  Heights in inches of the singers in the New York Choral Society in
  1979.  The data are grouped according to voice part.  The vocal range
  for each voice part increases in pitch according to the following
  order: Bass 2, Bass 1, Tenor 2, Tenor 1, Alto 2, Alto 1, Soprano 2,
  Soprano 1.
}

\usage{singer}

\format{
  A data frame with 235 observations on the following 2 variables.
  \describe{
    \item{height}{Height in inches of the singers.}
    \item{voice.part}{(Unordered) factor with levels "\code{Bass 2}",
      "\code{Bass 1}", "\code{Tenor 2}", "\code{Tenor 1}", "\code{Alto 2}",
      "\code{Alto 1}", "\code{Soprano 2}", "\code{Soprano 1}".}
  }
}

\source{
  Chambers, J.M., W. S. Cleveland, B. Kleiner, and P. A. Tukey. (1983).
  \emph{Graphical Methods for Data Analysis}.  Chapman and Hall, New York.
}

\author{
  Documentation contributed by Kevin Wright.
}

\references{
  Cleveland, William S. (1993) \emph{Visualizing Data}.  Hobart Press,
  Summit, New Jersey.
}

\examples{
# Separate histogram for each voice part (Figure 1.2 from Cleveland)
histogram(~ height | voice.part,
          data = singer,
          aspect=1,
          layout = c(2, 4), 
          nint=15,
          xlab = "Height (inches)")

# Quantile-Quantile plot (Figure 2.11 from Cleveland)
qqmath(~ height | voice.part,
       data=singer,
       aspect=1, 
       layout=c(2,4),
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.grid()
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       },
       xlab = "Unit Normal Quantile",
       ylab="Height (inches)")
}

\keyword{datasets}
