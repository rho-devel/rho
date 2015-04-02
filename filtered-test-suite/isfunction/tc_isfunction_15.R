expected <- TRUE            
test(id=110, code={            
argv <- list(function (x, width = 0.9 * getOption("width"), indent = 0,             
    exdent = 0, prefix = "", simplify = TRUE, initial = prefix)             
{            
    if (!is.character(x))             
        x <- as.character(x)            
    indentString <- paste(rep.int(" ", indent), collapse = "")            
    exdentString <- paste(rep.int(" ", exdent), collapse = "")            
    y <- list()            
    UB <- TRUE            
    if (all(Encoding(x) == "UTF-8"))             
        UB <- FALSE            
    else {            
        enc <- Encoding(x) %in% c("latin1", "UTF-8")            
        if (length(enc))             
            x[enc] <- enc2native(x[enc])            
    }            
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE, useBytes = UB),             
        strsplit, "[ \t\n]", perl = TRUE, useBytes = UB)            
    for (i in seq_along(z)) {            
        yi <- character()            
        for (j in seq_along(z[[i]])) {            
            words <- z[[i]][[j]]            
            nc <- nchar(words, type = "w")            
            if (anyNA(nc)) {            
                nc0 <- nchar(words, type = "b")            
                nc[is.na(nc)] <- nc0[is.na(nc)]            
            }            
            if (any(nc == 0L)) {            
                zLenInd <- which(nc == 0L)            
                zLenInd <- zLenInd[!(zLenInd %in% (grep("[.?!][)\"']{0,1}$",             
                  words, perl = TRUE, useBytes = TRUE) + 1L))]            
                if (length(zLenInd)) {            
                  words <- words[-zLenInd]            
                  nc <- nc[-zLenInd]            
                }            
            }            
            if (!length(words)) {            
                yi <- c(yi, "", initial)            
                next            
            }            
            currentIndex <- 0L            
            lowerBlockIndex <- 1L            
            upperBlockIndex <- integer()            
            lens <- cumsum(nc + 1L)            
            first <- TRUE            
            maxLength <- width - nchar(initial, type = "w") -             
                indent            
            while (length(lens)) {            
                k <- max(sum(lens <= maxLength), 1L)            
                if (first) {            
                  first <- FALSE            
                  maxLength <- width - nchar(prefix, type = "w") -             
                    exdent            
                }            
                currentIndex <- currentIndex + k            
                if (nc[currentIndex] == 0L)             
                  upperBlockIndex <- c(upperBlockIndex, currentIndex -             
                    1L)            
                else upperBlockIndex <- c(upperBlockIndex, currentIndex)            
                if (length(lens) > k) {            
                  if (nc[currentIndex + 1L] == 0L) {            
                    currentIndex <- currentIndex + 1L            
                    k <- k + 1L            
                  }            
                  lowerBlockIndex <- c(lowerBlockIndex, currentIndex +             
                    1L)            
                }            
                if (length(lens) > k)             
                  lens <- lens[-seq_len(k)] - lens[k]            
                else lens <- NULL            
            }            
            nBlocks <- length(upperBlockIndex)            
            s <- paste0(c(initial, rep.int(prefix, nBlocks -             
                1L)), c(indentString, rep.int(exdentString, nBlocks -             
                1L)))            
            initial <- prefix            
            for (k in seq_len(nBlocks)) s[k] <- paste0(s[k],             
                paste(words[lowerBlockIndex[k]:upperBlockIndex[k]],             
                  collapse = " "))            
            yi <- c(yi, s, prefix)            
        }            
        y <- if (length(yi))             
            c(y, list(yi[-length(yi)]))            
        else c(y, "")            
    }            
    if (simplify)             
        y <- as.character(unlist(y))            
    y            
})            
do.call('is.function', argv);            
},  o = expected);            
            
