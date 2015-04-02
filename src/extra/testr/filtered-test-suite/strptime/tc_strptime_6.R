expected <- eval(parse(text="structure(list(sec = c(0, NA), min = c(0L, NA), hour = c(0L, NA), mday = c(6L, NA), mon = c(10L, NA), year = c(107L, NA), wday = c(2L, NA), yday = c(309L, NA), isdst = c(0L, -1L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"2007-11-06\", NA), \"%Y-%m-%d\", \"\")"));  
.Internal(`strptime`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

