\name{G_axis.default}
\alias{axis.default}
\alias{xscale.components.default}
\alias{yscale.components.default}
\title{Default axis annotation utilities}

\description{
  Lattice funtions provide control over how the plot axes are annotated
  through a common interface.  There are two levels of control.  The
  \code{xscale.components} and \code{yscale.components} arguments can be
  functions that determine tick mark locations and labels given a
  packet.  For more direct control, the \code{axis} argument can be a
  function that actually draws the axes.  The functions documented here
  are the defaults for these arguments.  They can additonally be used as
  components of user written replacements.
}

\usage{
xscale.components.default(lim,
                          packet.number = 0,
                          packet.list = NULL,
                          top = TRUE,
                          \dots)
yscale.components.default(lim,
                          packet.number = 0,
                          packet.list = NULL,
                          right = TRUE,
                          \dots)
axis.default(side = c("top", "bottom", "left", "right"),
             scales, components, as.table,
             labels = c("default", "yes", "no"),
             ticks = c("default", "yes", "no"),
             \dots, prefix)

}

\arguments{

  \item{lim}{ the range of the data in that packet (data subset
    corresponding to a combination of levels of the conditioning
    variable).  The range is not necessarily numeric; e.g. for factors,
    they could be character vectors representing levels, and for the
    various date-time representations, they could be vectors of length 2
    with the corresponding class. }
  \item{packet.number}{ which packet (counted according to the packet
    order, described in \code{\link{print.trellis}}) is being
    processed.  In cases where all panels have the same limits, this
    function is called only once (rather than once for each packet), in
    which case this argument will have the value \code{0}.
  }
  \item{packet.list}{ list, as long as the number of packets, giving
    all the actual packets.  Specifically, each component is the
    list of arguments given to the panel function when and if that
    packet is drawn in a panel.  (This has not yet been implemented.) }
  \item{top, right}{ the value of the \code{top} and \code{right}
    components of the result, as appropriate.  See below for
    interpretation.}
  \item{side}{ on which side the axis is to be drawn.  The usual partial
    matching rules apply.  }
  \item{scales}{ the appropriate component of the \code{scales} argument
    supplied to the high level function, suitably standardized. }
  \item{components}{ list, similar to those produced by
    \code{xscale.components.default} and
    \code{yscale.components.default}.  }
  \item{as.table}{ the \code{as.table} argument in the high level
    function.  }
  \item{labels}{ whether labels are to be drawn.  By default, the rules
    determined by \code{scales} are used.  }
  \item{ticks}{ whether labels are to be drawn.  By default, the rules
    determined by \code{scales} are used.  }
  \item{\dots}{ many other arguments may be supplied, and are passed on
    to other internal functions. }
  \item{prefix}{
    A character string identifying the plot being drawn (see
    \code{\link{print.trellis}}).  Used to retrieve location of current
    panel in the overall layout, so that axes can be drawn
    appropriately.
  }
}

\value{

  \code{xscale.components.default} and \code{yscale.components.default}
  return a list of the form suitable as the \code{components} argument
  of \code{axis.default}.  Valid components in the return value of
  \code{xscale.components.default} are:

  \item{\code{num.limit}}{ A numeric limit for the box. }
  \item{\code{bottom}}{ A list with two elements, \code{ticks} and
    \code{labels}.  \code{ticks} must be a list with components
    \code{at} and \code{tck} which give the location and lengths of tick
    marks.  \code{tck} can be a vector, and will be recycled to be as
    long as \code{at}.  \code{labels} must be a list with components
    \code{at}, \code{labels}, and \code{check.overlap}.  \code{at} and
    \code{labels} give the location and labels of the tick labels; this
    is usually the same as the location of the ticks, but is not
    required to be so.  \code{check.overlap} is a logical flag
    indicating whether overlapping of labels should be avoided by
    omitting some of the labels while rendering.  }
  \item{\code{top}}{ This can be a logical flag; if \code{TRUE},
    \code{top} is treated as being the same as \code{bottom}; if
    \code{FALSE}, axis annotation for the top axis is omitted.
    Alternatively, \code{top} can be a list like \code{bottom}.
  }

  Valid components in the return value of
  \code{yscale.components.default} are \code{left} and \code{right}.
  Their interpretations are analogous to (respectively) the
  \code{bottom} and \code{top} components described above.
  
}

\details{

  These functions are part of a new API introduced in lattice 0.14 to
  provide the user more control over how axis annotation is done.  While
  the API has been designed in anticipation of use that was previously
  unsupported, the implementation has initially focused on reproducing
  existing capabilities, rather than test new features.  At the time of
  writing, several features are unimplemented.  If you require them,
  please contact the maintainer.

}

\examples{

str(xscale.components.default(c(0, 1)))

set.seed(36872)
rln <- rlnorm(100)

densityplot(rln, 
            scales = list(x = list(log = 2), alternating = 3),
            xlab = "Simulated lognormal variates",
            xscale.components = function(...) {
                ans <- xscale.components.default(...)
                ans$top <- ans$bottom
                ans$bottom$labels$labels <- parse(text = ans$bottom$labels$labels)
                ans$top$labels$labels <-
                    if (require(MASS))
                        fractions(2^(ans$top$labels$at))
                    else
                        2^(ans$top$labels$at)
                ans
            })


## Direct use of axis to show two temperature scales (Celcius and
## Fahrenheit).  This does not work for multi-row plots, and doesn't
## do automatic allocation of space


F2C <- function(f) 5 * (f - 32) / 9 
C2F <- function(c) 32 + 9 * c / 5 

axis.CF <-
    function(side, ...) 
{
    ylim <- current.panel.limits()$ylim
    switch(side,
           left = {
               prettyF <- pretty(ylim)
               labF <- parse(text = sprintf("\%s ~ degree * F", prettyF))
               panel.axis(side = side, outside = TRUE,
                          at = prettyF, labels = labF)
           },
           right = {
               prettyC <- pretty(F2C(ylim))
               labC <- parse(text = sprintf("\%s ~ degree * C", prettyC))
               panel.axis(side = side, outside = TRUE,
                          at = C2F(prettyC), labels = labC)
           },
           axis.default(side = side, ...))
}

xyplot(nhtemp ~ time(nhtemp), aspect = "xy", type = "o",
       scales = list(y = list(alternating = 3)),
       axis = axis.CF, xlab = "Year", ylab = "Temperature", 
       main = "Yearly temperature in New Haven, CT")

## version using yscale.components

yscale.components.CF <-
    function(...)
{
    ans <- yscale.components.default(...)
    ans$right <- ans$left
    ans$left$labels$labels <-
        parse(text = sprintf("\%s ~ degree * F", ans$left$labels$at))
    prettyC <- pretty(F2C(ans$num.limit))
    ans$right$ticks$at <- C2F(prettyC)
    ans$right$labels$at <- C2F(prettyC)
    ans$right$labels$labels <-
        parse(text = sprintf("\%s ~ degree * C", prettyC))
    ans
}
      

xyplot(nhtemp ~ time(nhtemp), aspect = "xy", type = "o",
       scales = list(y = list(alternating = 3)),
       yscale.components = yscale.components.CF,
       xlab = "Year", ylab = "Temperature", 
       main = "Yearly temperature in New Haven, CT")


}


\seealso{
  \code{\link{Lattice}},
  \code{\link{xyplot}}, 
  \code{\link{print.trellis}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

