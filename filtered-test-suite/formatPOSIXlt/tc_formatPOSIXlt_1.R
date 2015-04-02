expected <- eval(parse(text="c(\"2005-01-01\", \"2006-01-01\", \"2007-01-01\", \"2008-01-01\", \"2009-01-01\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = c(0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L, 0L), mday = c(1L, 1L, 1L, 1L, 1L), mon = c(0L, 0L, 0L, 0L, 0L), year = 105:109, wday = c(6L, 0L, 1L, 2L, 4L), yday = c(0L, 0L, 0L, 0L, 0L), isdst = c(0L, 0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\"), \"%Y-%m-%d\", FALSE)"));  
.Internal(`format.POSIXlt`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

