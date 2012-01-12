\name{solder}
\alias{solder}
\title{Soldering of Components on Printed-Circuit Boards}
\description{
  The \code{solder} data frame has 720 rows and 6 columns, representing
  a balanced subset of a designed experiment varying 5 factors on the
  soldering of components on printed-circuit boards.
}
\usage{
solder
}
\format{
This data frame contains the following columns:
\describe{
\item{\code{Opening}}{
a factor with levels
\code{L} 
\code{M} 
\code{S}
indicating the amount of clearance around the mounting pad.
}
\item{\code{Solder}}{
a factor with levels
\code{Thick} 
\code{Thin}
giving the thickness of the solder used.
}
\item{\code{Mask}}{
a factor with levels
\code{A1.5} 
\code{A3} 
\code{B3} 
\code{B6}
indicating the type and thickness of mask used.
}
\item{\code{PadType}}{
a factor with levels
\code{D4} 
\code{D6} 
\code{D7} 
\code{L4} 
\code{L6} 
\code{L7} 
\code{L8} 
\code{L9} 
\code{W4} 
\code{W9}
giving the size and geometry of the mounting pad.
}
\item{\code{Panel}}{
 \code{1:3} indicating the panel on a board being tested.
}
\item{\code{skips}}{
a numeric vector giving the number of visible solder skips.
}
}}
\source{
   John M. Chambers and Trevor J. Hastie eds. (1992)
  \emph{Statistical Models in S}, Wadsworth and Brooks/Cole,
  Pacific Grove, CA 1992.
}
\examples{
fit <- rpart(skips ~ Opening + Solder + Mask + PadType + Panel,
       data=solder, method='anova')
summary(residuals(fit))
plot(predict(fit),residuals(fit))
}
\keyword{datasets}
