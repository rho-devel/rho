expected <- eval(parse(text="\"1969-12-31 19:00:00 EST\""));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 19L, mday = 31L, mon = 11L, year = 69L, wday = 3L, yday = 364L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")), \"%Y-%m-%d %H:%M:%S\", TRUE)"));      
.Internal(format.POSIXlt(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

