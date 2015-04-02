expected <- eval(parse(text="c(\"2001-01-01\", NA, NA, \"2004-10-26\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = c(0, NA, NA, 0), min = c(0L, NA, NA, 0L), hour = c(0L, NA, NA, 0L), mday = c(1L, NA, NA, 26L), mon = c(0L, NA, NA, 9L), year = c(101L, NA, NA, 104L), wday = c(1L, NA, NA, 2L), yday = c(0L, NA, NA, 299L), isdst = c(0L, -1L, -1L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\"), \"%Y-%m-%d\", FALSE)"));      
.Internal(format.POSIXlt(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

