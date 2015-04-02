expected <- eval(parse(text="structure(list(sec = c(0, 0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L, 0L, 0L), mday = c(1L, 1L, 1L, 1L, 1L, 1L), mon = c(0L, 0L, 0L, 0L, 0L, 0L), year = -10:-5, wday = c(3L, 4L, 5L, 0L, 1L, 2L), yday = c(0L, 0L, 0L, 0L, 0L, 0L), isdst = c(0L, 0L, 0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(\"1890/01/01\", \"1891/01/01\", \"1892/01/01\", \"1893/01/01\", \"1894/01/01\", \"1895/01/01\"), \"%Y/%m/%d\", \"\")"));      
.Internal(strptime(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

