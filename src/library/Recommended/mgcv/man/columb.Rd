\name{columb}
\alias{columb}
\alias{columb.polys}
\docType{data}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Reduced version of Columbus OH crime data}

\description{By district crime data from Columbus OH, together with polygons describing district shape. Useful for 
illustrating use of simple Markov Random Field smoothers.
}
\usage{
data(columb)
data(columb.polys)
}
%- maybe also `usage' for other objects documented here.

\format{ \code{columb} is a 49 row data frame with the following columns 
\describe{
\item{area}{land area of district}
\item{home.value}{housing value in 1000USD.}
\item{income}{household income in 1000USD.}
\item{crime}{residential burglaries and auto thefts per 1000 households.}
\item{open.space}{measure of open space in district.}
\item{district}{code identifying district, and matching \code{names(columb.polys)}. }
}
\code{columb.polys} contains the polygons defining the areas in the format described below.
} 


\details{The data frame \code{columb} relates to the districts whose boundaries are coded in \code{columb.polys}.
\code{columb.polys[[i]]} is a 2 column matrix, containing the vertices of the polygons defining the boundary of the ith 
district. \code{columb.polys[[2]]} has an artificial hole inserted to illustrate how holes in districts can be spefified. Different polygons defining the boundary of a district are separated by NA rows in \code{columb.polys[[1]]}, 
and a polygon enclosed within another is treated as a hole in that region (a hole should never come first). 
\code{names(columb.polys)} matches \code{columb$district} (order unimportant).

}

\source{
The data are adapted from the \code{columbus} example in the \code{spdep} package, where the original source is given as: 

Anselin, Luc. 1988. Spatial econometrics: methods and models. Dordrecht: Kluwer Academic, Table 12.1 p. 189.

}



\examples{
## see ?mrf help files
}








