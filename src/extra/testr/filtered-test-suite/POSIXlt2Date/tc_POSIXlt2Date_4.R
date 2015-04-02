expected <- eval(parse(text="structure(3287, class = \"Date\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 1L, mon = 0L, year = 79L, wday = 1L, yday = 0L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\")))"));     
.Internal(POSIXlt2Date(argv[[1]]));     
}, o=expected);     

