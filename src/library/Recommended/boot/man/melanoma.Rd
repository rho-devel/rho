\name{melanoma}
\alias{melanoma}
\title{
Survival from Malignant Melanoma
}
\description{
The \code{melanoma} data frame has 205 rows and 7 columns.


The data consist of measurements made on patients with malignant melanoma.
Each patient had their tumour removed by surgery at the Department of Plastic
Surgery, University Hospital of Odense, Denmark during the period 1962 to 1977.
The surgery consisted of complete removal of the tumour together with about
2.5cm of the surrounding skin.  Among the measurements taken were the thickness
of the tumour and whether it was ulcerated or not.  These are thought to be
important prognostic variables in that patients with a thick and/or ulcerated
tumour have an increased chance of death from melanoma.
Patients were followed until the end of 1977.  
}
\usage{
melanoma
}
\format{
This data frame contains the following columns:

\describe{
\item{\code{time}}{
Survival time in days since the operation, possibly censored.
}
\item{\code{status}}{
The patients status at the end of the study. 1 indicates that they had died
from melanoma, 2 indicates that they were still alive and 3 indicates that they
had died from causes unrelated to their melanoma.
}
\item{\code{sex}}{
The patients sex; 1=male, 0=female.
}
\item{\code{age}}{
Age in years at the time of the operation.
}
\item{\code{year}}{
Year of operation.
}
\item{\code{thickness}}{
Tumour thickness in mm.
}
\item{\code{ulcer}}{
Indicator of ulceration; 1=present, 0=absent.
}}}
\note{
This dataset is not related to the dataset in the \pkg{lattice} package
with the same name.
}
\source{
The data were obtained from

Andersen, P.K., Borgan, O., Gill, R.D. and Keiding, N. (1993)
\emph{Statistical Models Based on Counting Processes}. Springer-Verlag.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and Their Application}. Cambridge University Press.

Venables, W.N. and Ripley, B.D. (1994) 
\emph{Modern Applied Statistics with S-Plus}. Springer-Verlag.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
