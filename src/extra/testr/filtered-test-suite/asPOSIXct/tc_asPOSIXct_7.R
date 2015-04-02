expected <- eval(parse(text="NA_real_"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = NA_real_, min = NA_integer_, hour = NA_integer_, mday = NA_integer_, mon = NA_integer_, year = NA_integer_, wday = NA_integer_, yday = NA_integer_, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"), \"GMT\")"));  
.Internal(`as.POSIXct`(argv[[1]], argv[[2]]));  
}, o=expected);  

