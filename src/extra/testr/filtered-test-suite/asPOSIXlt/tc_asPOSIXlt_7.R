expected <- eval(parse(text="structure(list(sec = c(0, 0, 0, 0), min = c(0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L), mday = c(4L, 11L, 18L, 25L), mon = c(1L, 1L, 1L, 1L), year = c(102L, 102L, 102L, 102L), wday = c(1L, 1L, 1L, 1L), yday = c(34L, 41L, 48L, 55L), isdst = c(0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1012798800, 1013403600, 1014008400, 1014613200), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), \"\")"));      
.Internal(as.POSIXlt(argv[[1]], argv[[2]]));      
}, o=expected);      

