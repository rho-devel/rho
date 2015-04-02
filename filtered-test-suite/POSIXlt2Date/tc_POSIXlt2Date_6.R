expected <- eval(parse(text="structure(c(10957, 11048, 11139, 11231, 11323, 11413, 11504, 11596, 11688, 11778, 11869, 11961, 12053), class = \"Date\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 1L, mon = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36), year = 100L, wday = 6L, yday = 0L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\"))"));     
.Internal(POSIXlt2Date(argv[[1]]));     
}, o=expected);     

