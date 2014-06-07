# file nnet/R/multiedit.R
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
multiedit <- function(x, class, k=1, V=3, I=5, trace=TRUE)
{
     n1 <- length(class)
     class <- unclass(class)
     index <- seq_len(n1)
     pass <- lpass <- 0L
     repeat{
         if(n1 < 5*V) {
             warning("retained set is now too small to proceed")
             break
         }
	 pass <- pass + 1L
	 sub <- sample(V, length(class), replace=TRUE)
	 keep <- logical(length(class))
	 for (i in 1L:V){
	     train <- sub==i
	     test <- sub==(1 + i%%V)
	     keep[test] <- (knn(x[train, , drop=FALSE], x[test, , drop=FALSE],
		class[train],k) == class[test])
	 }
	 x <- x[keep, , drop=FALSE]; class <- class[keep]; index <- index[keep]
	 n2 <- length(class)
	 if(n2 < n1) lpass <- pass
	 if(lpass <= pass - I) break
	 n1 <- n2
	 if(trace) message(gettextf("pass %s size %d", pass, n2),
                           domain = NA)
     }
     index
}

condense <- function(train, class, store=sample(seq(n), 1), trace=TRUE)
{
     n <- length(class)
     bag <- rep(TRUE, n)
     bag[store] <- FALSE
     repeat {
        if(trace) print(seq(n)[!bag])
        if(sum(bag) == 0) break
        res <- knn1(train[!bag,,drop = FALSE], train[bag,,drop = FALSE], class[!bag])
        add <- res != class[bag]
        if(sum(add) == 0) break
        cand <- (seq(n)[bag])[add]
	if(length(cand) > 1L) cand <- sample(cand, 1L)
        bag[cand] <- FALSE
     }
     seq(n)[!bag]
}

reduce.nn <- function(train, ind, class)
{
     n <- length(class)
     rest <- seq(n)[-ind]
# this must be done iteratively, not simultaneously
     for(i in sample(ind)) {
	 res <- knn1(train[-c(rest,i),,drop=FALSE], train[c(rest,i),,drop=FALSE],
	             class[-c(rest,i)])
	 if(all(res == class[c(rest,i)])) rest <- c(rest,i)
     }
     seq(n)[-rest]
}

