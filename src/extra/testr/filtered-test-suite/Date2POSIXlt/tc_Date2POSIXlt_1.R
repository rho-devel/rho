expected <- eval(parse(text="structure(list(sec = c(0, 0), min = c(0L, 0L), hour = c(0L, 0L), mday = c(13L, 13L), mon = c(3L, 3L), year = c(110L, 110L), wday = c(2L, 2L), yday = c(102L, 102L), isdst = c(0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(14712, 14712), class = \"Date\"))"));     
.Internal(Date2POSIXlt(argv[[1]]));     
}, o=expected);     

