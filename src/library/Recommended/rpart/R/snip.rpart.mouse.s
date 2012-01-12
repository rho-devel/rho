# SCCS %W% %G%
#
#  Interactively snip off part of a tree
#

snip.rpart.mouse <- function(tree,
		      parms=paste(".rpart.parms", dev.cur(), sep = ".")) {
    xy <- rpartco(tree)
    toss <- NULL
    ff <- tree$frame
    if (exists(parms, envir=.GlobalEnv)) {
        parms <- get(parms, envir=.GlobalEnv)
	branch <- parms$branch
	}
    else branch <- 1

    node <- as.numeric(row.names(tree$frame))
    draw <- rpart.branch(xy$x,xy$y, node, branch)

    lastchoice <- 0
    while (length(choose <- identify(xy, n=1, plot=FALSE)) >0 ) {
	if (ff$var[choose] == '<leaf>') {
		cat("Terminal node -- try again\n")
		next
		}

	if (choose != lastchoice) {
	    # print out some info on the click
	    cat("node number:", node[choose], " n=", ff$n[choose], "\n")
	    cat("    response=", format(ff$yval[choose]))
	    if (is.null(ff$yval2)) cat ("\n")
	    else if (is.matrix(ff$yval2))
		  cat(" (", format(ff$yval2[choose,]), ")\n")
	    else  cat(" (", format(ff$yval2[choose]), ")\n")
	    cat("    Error (dev) = ", format(ff$dev[choose]), "\n")
	    lastchoice <- choose
	    }
	else {
	    # second click-- erase all of the descendants
	    #   (stolen from snip.tree)
	    id  <- node[choose]
	    id2 <- node
	    while (any(id2>1)) {
		id2 <- floor(id2/2)
		temp  <- (match(id2, id, nomatch=0) >0)
  	        id <- c(id, node[temp])
		id2[temp] <- 0
		}
	    temp <- match(id, node[ff$var != '<leaf>'], nomatch=0)
	    lines(c(draw$x[,temp]), c(draw$y[,temp]), col=0L)
	    toss <- c(toss, node[choose])
	    }
	}
    toss
    }
