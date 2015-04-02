expected <- eval(parse(text="structure(list(sec = 48.0000001192093, min = 52L, hour = 8L, mday = 31L, mon = 11L, year = 102L, wday = 2L, yday = 364L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(1041324768, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\"), \"GMT\")"));      
.Internal(as.POSIXlt(argv[[1]], argv[[2]]));      
}, o=expected);      

