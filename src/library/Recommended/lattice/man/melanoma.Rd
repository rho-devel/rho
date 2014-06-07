\name{H_melanoma}
\alias{melanoma}
\docType{data}
\title{ Melanoma skin cancer incidence }

\description{
  These data from the Connecticut Tumor Registry present
  age-adjusted numbers of melanoma skin-cancer incidences per
  100,000 people in Connectict for the years from 1936 to 1972.
}

\usage{melanoma}

\format{
  A data frame with 37 observations on the following 2 variables.
  \describe{
    \item{year}{Years 1936 to 1972.}
    \item{incidence}{Rate of melanoma cancer per 100,000 population.}
  }
}

\note{
  This dataset is not related to the \code{\link[boot]{melanoma}}
  dataset in the \bold{boot} package with the same name.

  The S-PLUS 6.2 help for the melanoma data says that the incidence rate is
  per \emph{million}, but this is not consistent with data found at the
  National Cancer Institute (\url{http://www.nci.nih.gov}).
}

\source{
    Houghton, A., E. W. Munster, and M. V. Viola.  (1978).  Increased
    Incidence of Malignant Melanoma After Peaks of Sunspot Activity.
    \emph{The Lancet}, \bold{8}, 759--760.
}

\author{
  Documentation contributed by Kevin Wright.
}

\references{
  Cleveland, William S. (1993) \emph{Visualizing Data}.  Hobart Press,
  Summit, New Jersey.
}

\examples{
# Time-series plot.  Figure 3.64 from Cleveland.
xyplot(incidence ~ year,
  data = melanoma,
	aspect = "xy",
	panel = function(x, y)
		panel.xyplot(x, y, type="o", pch = 16),
	ylim = c(0, 6),
	xlab = "Year",
	ylab = "Incidence")
}

\keyword{datasets}
