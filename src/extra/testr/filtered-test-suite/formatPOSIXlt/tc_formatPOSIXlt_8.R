expected <- eval(parse(text="\"07:37:11.303\""));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 11.3034093379974, min = 37L, hour = 7L, mday = 7L, mon = 11L, year = 113L, wday = 6L, yday = 340L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")), \"%H:%M:%OS3\", FALSE)"));      
.Internal(format.POSIXlt(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

