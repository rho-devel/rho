expected <- eval(parse(text="structure(list(sec = NA_real_, min = NA_integer_, hour = NA_integer_, mday = NA_integer_, mon = NA_integer_, year = NA_integer_, wday = NA_integer_, yday = NA_integer_, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"1970-01-01\", \"%Y-%m-%d %H:%M\", \"GMT\")"));  
.Internal(`strptime`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

