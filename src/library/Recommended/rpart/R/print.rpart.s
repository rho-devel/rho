#SCCS  @(#)print.rpart.s	1.15 06/06/01
print.rpart <- function(x, minlength=0, spaces=2, cp,
               digits=getOption("digits"), ...) {
    if(!inherits(x, "rpart")) stop("Not legitimate rpart object")
    if (!is.null(x$frame$splits)) x <- rpconvert(x)  #help for old objects

    if (!missing(cp)) x <- prune.rpart(x, cp=cp)
    frame <- x$frame
    ylevel <- attr(x, "ylevels")
    node <- as.numeric(row.names(frame))
    depth <- tree.depth(node)
    indent <- paste(rep(" ", spaces * 32L), collapse = "")
    #32 is the maximal depth
    if(length(node) > 1L) {
        indent <- substring(indent, 1L, spaces * seq(depth))
        indent <- paste(c("", indent[depth]), format(node), ")", sep = "")
    }
    else indent <- paste(format(node), ")", sep = "")

    tfun <- (x$functions)$print
    if (!is.null(tfun)) {
	if (is.null(frame$yval2))
		yval <- tfun(frame$yval,  ylevel, digits)
	else    yval <- tfun(frame$yval2,  ylevel, digits)
	}
    else yval <- format(signif(frame$yval, digits = digits))
    term <- rep(" ", length(depth))
    term[frame$var == "<leaf>"] <- "*"
    z <- labels(x, digits=digits, minlength=minlength, ...)
    n <- frame$n
    z <- paste(indent, z, n, format(signif(frame$dev, digits = digits)),
               yval, term)

    omit <- x$na.action
    if (length(omit))
    cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep="")
    else cat("n=", n[1L], "\n\n")

    #This is stolen, unabashedly, from print.tree
    if (x$method=="class")
         cat("node), split, n, loss, yval, (yprob)\n")
    else cat("node), split, n, deviance, yval\n")
    cat("      * denotes terminal node\n\n")

    cat(z, sep = "\n")
    return(invisible(x))
    #end of the theft
    }
