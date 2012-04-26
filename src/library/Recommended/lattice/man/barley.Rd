\name{H_barley}
\alias{barley}
\docType{data}
\title{ Yield data from a Minnesota barley trial }

\description{
  Total yield in bushels per acre for 10 varieties at 6 sites in each of
  two years.
}

\usage{barley}

\format{
  A data frame with 120 observations on the following 4 variables.
  \describe{
    \item{yield}{Yield (averaged across three blocks) in bushels/acre.}
    \item{variety}{Factor with levels \code{"Svansota"}, \code{"No. 462"},
      \code{"Manchuria"}, \code{"No. 475"}, \code{"Velvet"},
      \code{"Peatland"}, \code{"Glabron"}, \code{"No. 457"},
      \code{"Wisconsin No. 38"}, \code{"Trebi"}.}
    \item{year}{Factor with levels \code{1932}, \code{1931}}
    \item{site}{Factor with 6 levels:  \code{"Grand Rapids"},
      \code{"Duluth"}, \code{"University Farm"}, \code{"Morris"},
      \code{"Crookston"}, \code{"Waseca"}}
  }
}

\details{
  These data are yields in bushels per acre, of 10 varieties of barley
  grown in 1/40 acre plots at University Farm, St. Paul, and at the five
  branch experiment stations located at Waseca, Morris, Crookston, Grand
  Rapids, and Duluth (all in Minnesota).  The varieties were grown in
  three randomized blocks at each of the six  stations during 1931 and
  1932, different land being used each year of the test.

  Immer et al. (1934) present the data for each Year*Site*Variety*Block.
  The data here is the average yield across the three blocks.

  Immer et al. (1934) refer (once) to the experiment as being conducted
  in 1930 and 1931, then later refer to it (repeatedly) as being
  conducted in 1931 and 1932.  Later authors have continued the
  confusion.

  Cleveland (1993) suggests that the data for the Morris site may have
  had the years switched.
  
}

\source{
  Immer, R. F., H. K. Hayes, and LeRoy Powers. (1934).  Statistical
  Determination of Barley Varietal Adaptation.  \emph{Journal of the
    American Society of Agronomy}, \bold{26}, 403--419.
}

\author{
  Documentation contributed by Kevin Wright.
}

\references{
  Cleveland, William S. (1993) \emph{Visualizing Data}.  Hobart Press,
  Summit, New Jersey.

  Fisher, R. A. (1971) \emph{The Design of Experiments}.  Hafner, New
  York, 9th edition.
}

\seealso{\code{\link[MASS]{immer}} in the MASS package for data from the
  same experiment 
  (expressed as total yield for 3 blocks) for a subset of varieties.}

\examples{
# Graphic suggesting the Morris data switched the years 1931 and 1932
# Figure 1.1 from Cleveland
dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)
}

\keyword{datasets}
