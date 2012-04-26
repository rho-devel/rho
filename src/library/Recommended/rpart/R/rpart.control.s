#SCCS @(#)rpart.control.s	1.10 07/05/01
rpart.control <-
  function(minsplit=20, minbucket= round(minsplit/3), cp=.01,
	   maxcompete=4, maxsurrogate=5, usesurrogate=2, xval=10,
	   surrogatestyle =0, maxdepth=30, ... ) {

	if (maxcompete<0) {
	    warning("The value of maxcompete supplied is <0; the value 0 was used instead")
	    maxcompete <-0
	    }
	if (any(xval<0)) {
	    warning("The value of xval supplied is <0; the value 0 was used instead")
	    xval <-0
	    }
	if (maxdepth > 30) stop("Maximum depth is 30")
	if (maxdepth < 1)  stop("Maximum depth must be at least 1")

	if (missing(minsplit) && !missing(minbucket)) minsplit <- minbucket*3

	if((usesurrogate < 0) || (usesurrogate > 2)) {
	    warning("The value of usesurrogate supplied was out of range," ,
		    "the default value of 2 is used instead.")
	    usesurrogate <- 2
	    }
	if((surrogatestyle < 0) || (surrogatestyle > 1)) {
	    warning("The value of surrogatestyle supplied was out of range,",
		    "the default value of 0 is used instead.")
	    surrogatestyle <- 0
	    }

	# Because xval can be of length either 1 or n, and the C code
	#   refers to parameters by number, i.e., "opt[5]" in rpart.c,
	#   the xval parameter should always be last on the list.
	list(minsplit=minsplit, minbucket=minbucket, cp=cp,
	     maxcompete=maxcompete, maxsurrogate=maxsurrogate,
	     usesurrogate=usesurrogate,
	     surrogatestyle=surrogatestyle, maxdepth=maxdepth, xval=xval )
	}
