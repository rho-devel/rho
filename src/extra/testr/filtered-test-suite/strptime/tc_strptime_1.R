expected <- eval(parse(text="structure(list(sec = 0, min = 0L, hour = 0L, mday = 22L, mon = 3L, year = 108L, wday = 2L, yday = 112L, isdst = 1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"2008-04-22 09:45\", \"%Y-%m-%d\", \"\")"));  
.Internal(`strptime`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

