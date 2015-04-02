expected <- eval(parse(text="structure(c(11323, 11323, 11354, 11354, 11382, 11382, 11413, 11688, 11688, 11719, 11719, 11747, 11747, 11778), class = \"Date\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(sec = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), mday = 1, mon = c(0, 0.5, 1, 1.5, 2, 2.5, 3), year = c(101, 101, 101, 101, 101, 101, 101, 102, 102, 102, 102, 102, 102, 102), wday = c(3L, 0L, 6L, 4L, 2L, 0L, 5L, 3L, 1L, 6L, 4L), yday = c(9L, 111L, 12L, 24L, 36L, 48L, 60L, 72L, 84L, 96L, 108L), isdst = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\"))"));     
.Internal(POSIXlt2Date(argv[[1]]));     
}, o=expected);     

