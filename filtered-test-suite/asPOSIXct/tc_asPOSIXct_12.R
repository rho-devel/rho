expected <- eval(parse(text="c(1012626000, 1012798800, 1012971600, 1013144400, 1013317200, 1013490000, 1013662800, 1013835600, 1014008400, 1014181200, 1014354000, 1014526800, 1014699600, 1014872400, 1015045200, 1015218000)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = c(2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L, 26L, 28L, 30L, 32L), mon = 1L, year = 102L, wday = 6L, yday = 32L, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")), \"\")"));      
.Internal(as.POSIXct(argv[[1]], argv[[2]]));      
}, o=expected);      

