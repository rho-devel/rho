# file nnet/R/knn.R
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#
knn1 <- function(train, test, cl)
{
	train <- as.matrix(train)
	if(is.null(dim(test))) dim(test) <- c(1, length(test))
	test <- as.matrix(test)
        if(any(is.na(train)) || any(is.na(test)) || any(is.na(cl)))
            stop("no missing values are allowed")
	p <- ncol(train)
	ntr <- nrow(train)
	if(length(cl) != ntr) stop("'train' and 'class' have different lengths")
	nte <- nrow(test)
	if(ncol(test) != p) stop("dims of 'test' and 'train' differ")
	clf <- as.factor(cl)
	nc <- max(unclass(clf))
	res <- .C(VR_knn1,
		as.integer(ntr),
		as.integer(nte),
		as.integer(p),
		as.double(train),
		as.integer(unclass(clf)),
		as.double(test),
		res = integer(nte),
		integer(nc+1),
		as.integer(nc),
		d = double(nte)
		)$res
	factor(res, levels=seq_along(levels(clf)), labels=levels(clf))
}

knn <- function(train, test, cl, k=1, l=0, prob=FALSE, use.all=TRUE)
{
	train <- as.matrix(train)
	if(is.null(dim(test))) dim(test) <- c(1, length(test))
	test <- as.matrix(test)
        if(any(is.na(train)) || any(is.na(test)) || any(is.na(cl)))
            stop("no missing values are allowed")
	p <- ncol(train)
	ntr <- nrow(train)
	if(length(cl) != ntr) stop("'train' and 'class' have different lengths")
	if(ntr < k) {
            warning(gettextf("k = %d exceeds number %d of patterns", k, ntr),
                    domain = NA)
	   k <- ntr
	}
	if (k < 1)
            stop(gettextf("k = %d must be at least 1", k), domain = NA)
	nte <- nrow(test)
	if(ncol(test) != p) stop("dims of 'test' and 'train' differ")
	clf <- as.factor(cl)
	nc <- max(unclass(clf))
	Z <- .C(VR_knn,
		as.integer(k),
		as.integer(l),
		as.integer(ntr),
		as.integer(nte),
		as.integer(p),
		as.double(train),
		as.integer(unclass(clf)),
		as.double(test),
		res = integer(nte),
		pr = double(nte),
		integer(nc+1),
		as.integer(nc),
		as.integer(FALSE),
		as.integer(use.all)
		)
	res <- factor(Z$res, levels=seq_along(levels(clf)),labels=levels(clf))
	if(prob) attr(res, "prob") <- Z$pr
	res
}

knn.cv <- function(train, cl, k=1, l=0, prob=FALSE, use.all=TRUE)
{
	train <- as.matrix(train)
        if(any(is.na(train)) || any(is.na(cl)))
            stop("no missing values are allowed")
	p <- ncol(train)
	ntr <- nrow(train)
	if(length(cl) != ntr) stop("'train' and 'class' have different lengths")
	if(ntr-1 < k) {
            warning(gettextf("k = %d exceeds number %d of patterns", k, ntr-1),
                    domain = NA)
	   k <- ntr - 1
	}
	if (k < 1)
            stop(gettextf("k = %d must be at least 1", k), domain = NA)
	clf <- as.factor(cl)
	nc <- max(unclass(clf))
	Z <- .C(VR_knn,
		as.integer(k),
		as.integer(l),
		as.integer(ntr),
		as.integer(ntr),
		as.integer(p),
		as.double(train),
		as.integer(unclass(clf)),
		as.double(train),
		res = integer(ntr),
		pr = double(ntr),
		integer(nc+1),
		as.integer(nc),
		as.integer(TRUE),
		as.integer(use.all)
		)
	res <- factor(Z$res, levels=seq_along(levels(clf)),labels=levels(clf))
	if(prob) attr(res, "prob") <- Z$pr
	res
}
