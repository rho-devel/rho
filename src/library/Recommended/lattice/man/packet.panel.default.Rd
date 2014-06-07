\name{G_packet.panel.default}
\alias{packet.panel.default}

\title{Associating Packets with Panels}

\description{

  When a \code{"trellis"} object is plotted, panels are always drawn in
  an order such that columns vary the fastest, then rows and then pages.
  An optional function can be specified that determines, given the
  column, row and page and other relevant information, the packet (if
  any) which should be used in that panel.  The function documented here
  implements the default behaviour, which is to match panel order with
  packet order, determined by varying the first conditioning variable
  the fastest, then the second, and so on.  This matching is performed
  after any reordering and/or permutation of the conditioning variables.

}
\usage{

packet.panel.default(layout, condlevels, page, row, column,
                     skip, all.pages.skip = TRUE) 

}

\arguments{
  \item{layout}{ the \code{layout} argument in high level functions,
    suitably standardized.  }
  \item{condlevels}{ a list of levels of conditioning variables, after
    relevant permutations and/or reordering of levels }
  \item{page, row, column}{ the location of the panel in the coordinate
    system of pages, rows and columns.  }
  \item{skip}{ the \code{skip} argument in high level functions }
  \item{all.pages.skip}{ whether \code{skip} should be replicated over
    all pages.  If \code{FALSE}, \code{skip} will be replicated to be
    only as long as the number of positions on a page, and that
    template will be used for all pages. }
}

\value{

  A suitable combination of levels of the conditioning variables in the
  form of a numeric vector as long as the number of conditioning
  variables, with each element an integer indexing the levels of the
  corresponding variable.  Specifically, if the return value is
  \code{p}, then the \code{i}-th conditioning variable will have level
  \code{condlevels[[i]][p[i]]}.

}

\seealso{ \code{\link{Lattice}}, \code{\link{xyplot}} }

\examples{

packet.panel.page <- function(n)
{
    ## returns a function that when used as the 'packet.panel'
    ## argument in print.trellis plots page number 'n' only
    function(layout, page, ...) {
        stopifnot(layout[3] == 1)
        packet.panel.default(layout = layout, page = n, ...)
    }
}

data(mtcars)
HP <- equal.count(mtcars$hp, 6)
p <- 
    xyplot(mpg ~ disp | HP * factor(cyl),
           mtcars, layout = c(0, 6, 1))

print(p, packet.panel = packet.panel.page(1))
print(p, packet.panel = packet.panel.page(2))

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

