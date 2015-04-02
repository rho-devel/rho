expected <- eval(parse(text="structure(list(sec = 0, min = 0L, hour = 0L, mday = 6L, mon = 10L, year = 107L, wday = 2L, yday = 309L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"2007-11-06\", \"%Y-%m-%d\", \"GMT\")"));  
.Internal(`strptime`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

