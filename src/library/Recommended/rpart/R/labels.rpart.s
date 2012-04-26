# SCCS @(#)labels.rpart.s	1.5 07/17/01
# Make the nice labels used by print and summary
#   digits = obvious
#   minlength = 0 = don't abbrev factors
#               1 = use single letters
#               2+= the same arg as the "abbreviate" function
#   collapse = an oddly named argument
#              F = return a matrix with two columns, containing the labels of
#                    the left and right descendants of each node
#              T = return a vector of 1 column, with the label of the parent
#   pretty: for historical compatability
#               0   -> minlength =0
#              NULL -> minlength =1
#               T   -> minlength =4
#   ... = other args for abbreviate()
#
labels.rpart <- function(object, digits=4, minlength=1L, pretty,
			      collapse=TRUE, ...) {
    if (missing(minlength) && !missing(pretty)) {
	if (is.null(pretty)) minlength <- 1L
	else if (is.logical(pretty)) {
	    if (pretty) minlength <- 4L
	    else        minlength <- 0L
	    }
	else minlength <- 0L
	}

    ff <- object$frame
    n  <- nrow(ff)
    if (n==1) return("root")  #special case of no splits

    is.leaf <- (ff$var == "<leaf>")
    whichrow <- !is.leaf
    vnames <- ff$var[whichrow]  #the variable names for the primary splits

    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + 1*(!is.leaf)))
    irow  <- index[c(whichrow, FALSE)]     #we only care about the primary split
    ncat  <- object$splits[irow, 2L]

    # Now to work: first create labels for the left and right splits,
    #  but not for leaves of course
    #
    lsplit <- rsplit <- vector(mode='character', length= length(irow))

    if (any(ncat <2L)) {  # any continuous vars ?
	jrow <- irow[ncat <2L]
	cutpoint <- formatg(object$splits[jrow,4L], digits)
	temp1 <- (ifelse(ncat<0, "< ", ">="))[ncat <2L]
	temp2 <- (ifelse(ncat<0, ">=", "< "))[ncat <2L]
	lsplit[ncat<2L] <- paste(temp1, cutpoint, sep='')
	rsplit[ncat<2L] <- paste(temp2, cutpoint, sep='')
	}

    if (any(ncat >1)) { # any categorical variables ?
	xlevels <- attr(object, 'xlevels')
	#
	# jrow will be the row numbers of factors within lsplit and rsplit
	# crow the row number in "csplit"
	# and cindex the index on the "xlevels" list
	#
	jrow <- (seq_along(ncat))[ncat>1L]
	crow <- object$splits[irow[ncat>1L],4L]    #row number in csplit
	cindex <- (match(vnames, names(xlevels)))[ncat >1L]

	# Now, abbreviate the levels
	if (minlength == 1L) {
	    if (any(ncat>52L))
		warning("more than 52 levels in a predicting factor, truncated for printout")
	    xlevels <- lapply(xlevels,
			       function(z) {
				   k <- length(z)
				   k <- pmin(1L:k, 52L)
				   c(letters, LETTERS)[k]
                               })
	    }
	else if (minlength > 1L)
	    xlevels <- lapply(xlevels, abbreviate, minlength, ...)

	# Now tuck in the labels
	# I'll let some other clever person vectorize this
	for (i in 1L:(length(jrow))) {
	    j <- jrow[i]
	    splits <- object$csplit[crow[i],]
	    # splits will contain 1=left, 2=right, 3= neither
	    ltemp <- (1L:length(splits))[splits== 1L]
	    rtemp <- (1L:length(splits))[splits== 3L]
	    if (minlength==1L) {
		lsplit[j] <- paste((xlevels[[cindex[i]]])[ltemp], collapse='')
		rsplit[j] <- paste((xlevels[[cindex[i]]])[rtemp], collapse='')
		}
	    else {
		lsplit[j] <-paste((xlevels[[cindex[i]]])[ltemp], collapse=',')
		rsplit[j] <-paste((xlevels[[cindex[i]]])[rtemp], collapse=',')
		}
	    }
	}

    if (!collapse) {  #called by no routines that I know of
	ltemp <- rtemp <- rep("<leaf>", n)
	ltemp[whichrow] <- lsplit
	rtemp[whichrow] <- rsplit
	return(cbind(ltemp, rtemp))
	}

    lsplit <- paste(ifelse(ncat<2L, "", "="), lsplit, sep='')
    rsplit <- paste(ifelse(ncat<2L, "", "="), rsplit, sep='')

    #
    # Now match them up to node numbers
    #   The output will have one label per row of object$frame, each
    #   corresponding the the line segement joining this node to its parent
    #
    varname <- (as.character(vnames))
    node <- as.numeric(row.names(ff))
    parent <- match(node %/% 2, node[whichrow])
    odd <- (as.logical(node %%2))

    labels <- vector('character', length=n)
    labels[odd] <- paste(varname[parent[odd]], rsplit[parent[odd]], sep="")
    labels[!odd]<- paste(varname[parent[!odd]],lsplit[parent[!odd]], sep="")
    labels[1] <- "root"
    labels
    }
