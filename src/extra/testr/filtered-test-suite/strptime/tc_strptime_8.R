expected <- eval(parse(text="structure(list(sec = 0, min = 2L, hour = 2L, mday = 2L, mon = 1L, year = 102L, wday = 6L, yday = 32L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"2002-02-02 02:02\", \"%Y-%m-%d %H:%M\", \"\")"));      
.Internal(strptime(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

