\name{nitrofen}
\alias{nitrofen}
\title{
Toxicity of Nitrofen in Aquatic Systems
}
\description{
The \code{nitrofen} data frame has 50 rows and 5 columns.

Nitrofen is a herbicide that was used extensively for the control of 
broad-leaved and grass weeds in cereals and rice. Although it is relatively
non-toxic to adult mammals, nitrofen is a significant tetragen and mutagen.
It is also acutely toxic and reproductively toxic to cladoceran zooplankton.
Nitrofen is no longer in commercial use in the U.S., having been the first
pesticide to be withdrawn due to tetragenic effects.

The data here come from an experiment to measure the reproductive toxicity
of nitrofen on a species of zooplankton (\emph{Ceriodaphnia dubia}).  50 animals
were randomized into batches of 10 and each batch was put in a solution with
a measured concentration of nitrofen.  Then the number of live offspring in
each of the three broods to each animal was recorded.
}
\usage{
nitrofen
}
\format{
This data frame contains the following columns:
\describe{
\item{\code{conc}}{
The nitrofen concentration in the solution (mug/litre).
}
\item{\code{brood1}}{
The number of live offspring in the first brood.
}
\item{\code{brood2}}{
The number of live offspring in the second brood.
}
\item{\code{brood3}}{
The number of live offspring in the third brood.
}
\item{\code{total}}{
The total number of live offspring in the first three broods.
}}}
\source{
The data were obtained from

Bailer, A.J. and Oris, J.T. (1994) Assessing toxicity of pollutants in aquatic 
systems. In \emph{Case Studies in Biometry}. N. Lange, L. Ryan, L. Billard,
D. Brillinger, L. Conquest and J. Greenhouse (editors), 25--40. John Wiley.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
