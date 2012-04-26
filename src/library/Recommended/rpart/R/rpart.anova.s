#SCCS @(#)rpart.anova.s	1.4 05/02/01
rpart.anova <- function(y, offset, parms, wt) {
    if (!is.null(offset)) y <- y-offset
    list(y=y, parms=0L, numresp=1L, numy=1L,
	 summary= function(yval, dev, wt, ylevel, digits ) {
	     paste("  mean=", formatg(yval, digits),
		   ", MSE=" , formatg(dev/wt, digits),
		   sep='')
	     },
	 text= function(yval, dev, wt, ylevel, digits, n, use.n ) {
	     if(use.n) {paste(formatg(yval,digits),"\nn=", n,sep="")} else
	               {paste(formatg(yval,digits))}}

	 )
    }
