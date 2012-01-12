# SCCS @(#)rpconvert.s	1.3 06/08/01
# Convert from the orginial style rpart object to the newer
#  style object (the changes made when user-written splits were added)
#
rpconvert <- function(x)
{
    if (!inherits(x, "rpart"))
        stop("x does not appear to be an rpart object")
    ff <- x$frame
    if (is.null(ff$splits)) {
        # this appears to be a new style one already
	warning("x not converted")
	return(x)
    }
    ff$splits <- NULL
    ff$wt <- ff$n

    xlev <- attr(x, "xlevels")
    if (length(xlev) >0) {
	zz <- as.numeric(names(xlev))
	names(xlev) <- attr(x$terms, "term.labels")[zz]
	attr(x, "xlevels") <- xlev
    }

    if (x$method=="class") {
	temp <- cbind(ff$yval, ff$yval2, ff$yprob)
	dimnames(temp) <- NULL
	ff$yval2 <- temp
	ff$yprob <- NULL
	x$frame <- ff

	temp <- rpart.class(c(1,1,2,2), NULL, wt=c(1,1,1,1))#dummy call
	x$functions <- list(summary=temp$summary, print=temp$print,
			    text = temp$text)
    }

    else if (x$method=="anova") {
	x$frame <- ff

	temp <- rpart.anova(1:5, NULL, wt=rep(1,5))#dummy call
	x$functions <- list(summary=temp$summary, text = temp$text)
    }

    else {  #either exp or poisson (they have the same summary/text pair)
	ff$yval2 <- cbind(ff$yval, ff$yval2)
	x$frame <- ff

	temp <- rpart.poisson(1:5, NULL, wt=rep(1,5))#dummy call
	x$functions <- list(summary=temp$summary, text = temp$text)
    }

    class(x) <- "rpart"
    x
}
