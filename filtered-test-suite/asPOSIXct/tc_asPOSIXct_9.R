expected <- eval(parse(text="c(1208822400, 1208908800, 1208995200, 1209081600, 1209168000, 1209254400)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 22:27, mon = 3L, year = 108L, wday = 2L, yday = 112L, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"), \"GMT\")"));  
.Internal(`as.POSIXct`(argv[[1]], argv[[2]]));  
}, o=expected);  

