expected <- eval(parse(text="structure(list(sec = c(0, 0), min = c(0L, 0L), hour = c(0L, 0L), mday = c(1L, 1L), mon = c(0L, 0L), year = c(70L, 70L), wday = c(4L, 4L), yday = c(0L, 0L), isdst = c(0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(FALSE, FALSE), class = \"Date\"))"));     
.Internal(Date2POSIXlt(argv[[1]]));     
}, o=expected);     

