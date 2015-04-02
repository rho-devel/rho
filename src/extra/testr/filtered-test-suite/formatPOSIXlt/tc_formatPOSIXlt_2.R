expected <- eval(parse(text="\"2014-03-17\""));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = 10.7712235450745, min = 48L, hour = 14L, mday = 17L, mon = 2L, year = 114L, wday = 1L, yday = 75L, isdst = 1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")), \"%Y-%m-%d\", FALSE)"));  
.Internal(`format.POSIXlt`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

