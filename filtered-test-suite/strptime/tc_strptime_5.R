expected <- eval(parse(text="structure(list(sec = 0, min = 0L, hour = 0L, mday = 1L, mon = 0L, year = 70L, wday = 4L, yday = 0L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"1970-01-01\", \"%Y-%m-%d\", \"GMT\")"));  
.Internal(`strptime`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

