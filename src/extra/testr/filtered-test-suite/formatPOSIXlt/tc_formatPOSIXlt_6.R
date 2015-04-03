expected <- eval(parse(text="c(\"1890\", \"1891\", \"1892\", \"1893\", \"1894\", \"1895\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = c(0, 0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L, 0L, 0L), mday = 6:11, mon = 0:5, year = -10:-5, wday = c(1L, 6L, 2L, 0L, 4L, 2L), yday = c(5L, 37L, 67L, 98L, 129L, 161L), isdst = c(0L, 0L, 0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\"), \"%Y\", FALSE)"));      
.Internal(format.POSIXlt(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

