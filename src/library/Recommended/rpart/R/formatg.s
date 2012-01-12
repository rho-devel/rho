# SCCS  @(#)formatg.s	1.3 06/06/01
# format a set of numbers using C's "g" format
#  It is applied on an element by element basis, which is more
#  appropriate for rpart output than the standard Splus format()
#  command.
# For instance if x=(123, 1.23, .00123)
#	  format(x) = "123.00000", "1.23000", "0.00123"
#  but formatg does not add all of those zeros to the first two numbers
#
formatg <- function(x, digits= unlist(options('digits')),
		         format= paste("%.", digits, "g", sep='')) {
    if (!is.numeric(x)) stop("x must be a numeric vector")

    n <- length(x)
    #
    # the resultant strings could be up to 8 characters longer,
    #   assume that digits =4,  -0.dddde+104 is a worst case, where
    #   dddd are the 4 significant digits.
##     dummy  <- paste(rep(" ", digits+8), collapse='')
##     temp <- .C(C_formatg, as.integer(n),
## 	                  as.double(x),
##                           rep(format,n),
##                           out= rep(dummy, n), NAOK=TRUE)$out
    temp <- sprintf(format, x)
    if (is.matrix(x)) matrix(temp, nrow=nrow(x))
    else temp
    }
