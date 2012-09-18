#SCCS  @(#)rpart.matrix.s	1.6 04/02/01
#
# This differs from tree.matrix in xlevels -- we don't keep NULLS in
#   the list for all of the non-categoricals
#
rpart.matrix <- function(frame)
    {
    if(!inherits(frame, "data.frame"))
	    return(as.matrix(frame))
    frame$"(weights)" <- NULL
    terms <- attr(frame, "terms")
    if(is.null(terms)) predictors <- names(frame)
    else {
	a <- attributes(terms)
	predictors <- as.character(a$variables)[-1L] # R change
        ## and this might include backquotes
        predictors <- sub("^`(.*)`$", "\\1", predictors)
	removals <- NULL
	if((TT <- a$response) > 0L) {
	    removals <- TT
	    frame[[predictors[TT]]] <- NULL
	    }
	if(!is.null(TT <- a$offset)) {
	    removals <- c(removals, TT)
	    frame[[predictors[TT]]] <- NULL
	    }
	if(!is.null(removals)) predictors <- predictors[ - removals]
        labels <- a$term.labels
	if(abs(length(labels)-length(predictors))>0)
	  predictors <- predictors[match(labels,predictors)]
	}

    factors <- sapply(frame, function(x) !is.null(levels(x)))
    characters <- sapply(frame, is.character)
    if(any(factors | characters)) {
	# change characters to factors
	for (preds in predictors[characters])
		frame[[preds]] <- as.factor(frame[[preds]])
        factors <- factors | characters
        column.levels <- lapply(frame[factors], levels)

	# Now make them numeric
	for (preds in predictors[factors])
	     frame[[preds]] <- as.numeric(frame[[preds]])
	x <- as.matrix(frame)
	attr(x, "column.levels") <- column.levels
	}
    else x <- as.matrix(frame[predictors])
    class(x) <- "rpart.matrix"
    x
    }


