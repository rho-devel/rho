expected <- eval(parse(text="1233446400"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 1, mon = 1L, year = 109L, wday = 0L, yday = 31L, isdst = -1), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\"), \"UTC\")"));  
.Internal(`as.POSIXct`(argv[[1]], argv[[2]]));  
}, o=expected);  

