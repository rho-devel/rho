expected <- eval(parse(text="structure(list(sec = c(0, NA, NA, 0), min = c(0L, NA, NA, 0L), hour = c(0L, NA, NA, 0L), mday = c(1L, NA, NA, 26L), mon = c(0L, NA, NA, 9L), year = c(101L, NA, NA, 104L), wday = c(1L, NA, NA, 2L), yday = c(0L, NA, NA, 299L), isdst = c(0L, -1L, -1L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(11323, NA, NA, 12717), class = \"Date\"))"));     
.Internal(Date2POSIXlt(argv[[1]]));     
}, o=expected);     

