expected <- eval(parse(text="structure(list(sec = 0, min = 0L, hour = 0L, mday = 1L, mon = 0L, year = 101L, wday = 1L, yday = 0L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(11323.9154302836, class = \"Date\"))"));     
.Internal(Date2POSIXlt(argv[[1]]));     
}, o=expected);     

