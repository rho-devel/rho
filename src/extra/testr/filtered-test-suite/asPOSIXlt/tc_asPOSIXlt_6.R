expected <- eval(parse(text="structure(list(sec = 0, min = 2L, hour = 2L, mday = 2L, mon = 1L, year = 1102L, wday = 2L, yday = 33L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(32569542120, class = c(\"POSIXct\", \"POSIXt\")), \"\")"));      
.Internal(as.POSIXlt(argv[[1]], argv[[2]]));      
}, o=expected);      

