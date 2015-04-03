expected <- eval(parse(text="1012194000"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = -3L, mon = 1L, year = 102L, wday = 6L, yday = 32L, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")), \"\")"));      
.Internal(as.POSIXct(argv[[1]], argv[[2]]));      
}, o=expected);      

