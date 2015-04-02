expected <- eval(parse(text="-2366755200"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 1L, mon = 0L, year = -5L, wday = 2L, yday = 0L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"), \"GMT\")"));      
.Internal(as.POSIXct(argv[[1]], argv[[2]]));      
}, o=expected);      

