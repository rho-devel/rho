expected <- eval(parse(text="\"2014-03-17 18:47:59 GMT\""));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = 59.7693939208984, min = 47L, hour = 18L, mday = 17L, mon = 2L, year = 114L, wday = 1L, yday = 75L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"), \"%Y-%m-%d %H:%M:%S\", TRUE)"));  
.Internal(`format.POSIXlt`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

