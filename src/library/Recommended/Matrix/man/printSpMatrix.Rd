\name{printSpMatrix}
\alias{formatSpMatrix}
\alias{printSpMatrix}
\alias{printSpMatrix2}
\title{Format and Print Sparse Matrices Flexibly}
\description{
  Format and print sparse matrices flexibly. These are the \dQuote{workhorses} used by
  the \code{\link{format}}, \code{\link{show}} and \code{\link{print}}
  methods for sparse matrices.  If \code{x} is large,
  \code{printSpMatrix2(x)} calls \code{printSpMatrix()} twice, namely,
  for the first and the last few rows, suppressing those in between, and
  also suppresses columns when \code{x} is too wide.

 \code{printSpMatrix()} basically prints the result of
 \code{formatSpMatrix()}.
}
\usage{
formatSpMatrix(x, digits = NULL, maxp = 1e9,
               cld = getClassDef(class(x)), zero.print = ".",
               col.names, note.dropping.colnames = TRUE,
               align = c("fancy", "right"))

printSpMatrix(x, digits = NULL, maxp = getOption("max.print"),
              cld = getClassDef(class(x)),
              zero.print = ".", col.names, note.dropping.colnames = TRUE,
              col.trailer = "",
              align = c("fancy", "right"))

printSpMatrix2(x, digits = NULL, maxp = getOption("max.print"),
              zero.print = ".", col.names, note.dropping.colnames = TRUE,
              suppRows = NULL, suppCols = NULL,
              col.trailer = if(suppCols) "......" else "",
              align = c("fancy", "right"))
}
\arguments{
  \item{x}{an \R object inheriting from class \code{\linkS4class{sparseMatrix}}.}
  \item{digits}{significant digits to use for printing, see
    \code{\link{print.default}}, the default, \code{\link{NULL}},
    corresponds to using \code{\link{getOption}("digits")}.}
  \item{maxp}{integer, default from \code{\link{options}(max.print)},
    influences how many entries of large matrices are printed at all.}
  \item{cld}{the class definition of \code{x}; must be equivalent to
    \code{\link{getClassDef}(class(x))} and exists mainly for possible
    speedup.}
  \item{zero.print}{character which should be printed for
    \emph{structural} zeroes.  The default \code{"."} may occasionally
    be replaced by \code{" "} (blank); using \code{"0"} would look
    almost like \code{print()}ing of non-sparse matrices.}
  \item{col.names}{logical or string specifying if and how column names of
    \code{x} should be printed, possibly abbreviated.  The default is
    taken from \code{\link{options}("sparse.colnames")} if that is set, otherwise
    \code{FALSE} unless there are less than ten columns.  When
    \code{TRUE} the full column names are printed.\cr
    When \code{col.names} is a string beginning with \code{"abb"} or
    \code{"sub"} and ending with an integer \code{n} (i.e., of the form
    \code{"abb... <n>"}),
    the column names are \code{\link{abbreviate}()}d or
    \code{\link{substring}()}ed to (target) length \code{n}, see the examples.
  }
  \item{note.dropping.colnames}{logical specifying, when
    \code{col.names} is \code{FALSE} if the dropping of the column names
    should be noted, \code{TRUE} by default.}
  \item{col.trailer}{a string to be appended to the right of each
    column; this is typically made use of by \code{\link{show}(<sparseMatrix>)}
    only, when suppressing columns.}
  \item{suppRows, suppCols}{logicals or \code{NULL}, for
    \code{printSpMatrix2()} specifying if rows or columns should be
    suppressed in printing.  If \code{NULL}, sensible defaults are
    determined from \code{\link{dim}(x)} and
    \code{\link{options}(c("width", "max.print"))}.
    Setting both to \code{FALSE} may be a very bad idea.}
  \item{align}{a string specifying how the \code{zero.print} codes
    should be aligned, i.e., padded as strings.  The default,
    \code{"fancy"}, takes some effort to align the typical
    \code{zero.print = "."} with the position of \code{0}, i.e., the
    first decimal (one left of decimal point) of the numbers printed,
    whereas \code{align = "right"} just makes use of
    \code{\link{print}(*, right = TRUE)}.
  }
}
\details{
  \describe{
    \item{formatSpMatrix:}{
      If \code{x} is large, only the first rows making up the
      approximately first \code{maxp} entries is used, otherwise all of \code{x}.
      \code{\link{.formatSparseSimple}()} is applied to (a dense version
      of) the matrix.  Then, \code{\link{formatSparseM}} is used, unless
      in trivial cases or for sparse matrices without \code{x} slot.}
  }
}
\value{
  \item{formatSpMatrix()}{returns a character matrix with possibly empty
  column names, depending on \code{col.names} etc, see above.}

  \item{printSpMatrix*()}{return \code{x} \emph{invisibly}, see \code{\link{invisible}}.}
}
\author{Martin Maechler}
\seealso{the virtual class \code{\linkS4class{sparseMatrix}} and the
  classes extending it;  maybe \code{\link{sparseMatrix}} or
  \code{\link{spMatrix}} as simple constructors of such matrices.

  The underlying utilities \code{\link{formatSparseM}} and
  \code{.formatSparseSimple()} (on the same page).
}
\examples{
f1 <- gl(5, 3, labels = LETTERS[1:5])
X <- as(f1, "sparseMatrix")
X ## <==>  show(X)  <==>  print(X)
t(X) ## shows column names, since only 5 columns

X2 <- as(gl(12, 3, labels = paste(LETTERS[1:12],"c",sep=".")),
         "sparseMatrix")
X2
## less nice, but possible:
print(X2, col.names = TRUE) # use [,1] [,2] .. => does not fit

t(X2) # suppressing column names
print(t(X2), col.names=TRUE)
print(t(X2), zero.print = "", col.names="abbr. 1")
print(t(X2), zero.print = "-", col.names="substring 2")

\dontshow{% show() was slow in 0.9975-8 because of slow adjust="fancy"
op <- options(max.print = 25000, width = 80)
sink(print(tempfile()))
M <- Matrix(0, 10000, 100)
M[1,1] <- M[2,3] <- 3.14
stopifnot((st <- system.time(show(M)))[1] < 1.0) # only 0.09 on cmath-3
sink()
st
options(op)
}
}
\keyword{print}
