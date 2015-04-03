expected <- eval(parse(text="c(975628800, 978307200, 980985600, 983404800)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 1, mon = c(11, 12, 13, 14), year = 100L, wday = 0L, yday = 365L, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"), \"GMT\")"));      
.Internal(as.POSIXct(argv[[1]], argv[[2]]));      
}, o=expected);      

