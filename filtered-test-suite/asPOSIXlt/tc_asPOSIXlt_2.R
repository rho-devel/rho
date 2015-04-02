expected <- eval(parse(text="structure(list(sec = c(0, 0), min = c(0L, 0L), hour = c(19L, 19L), mday = c(31L, 31L), mon = c(11L, 11L), year = c(69L, 69L), wday = c(3L, 3L), yday = c(364L, 364L), isdst = c(0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(FALSE, FALSE), class = c(\"POSIXct\", \"POSIXt\")), \"\")"));      
.Internal(as.POSIXlt(argv[[1]], argv[[2]]));      
}, o=expected);      

