expected <- eval(parse(text="structure(list(sec = c(0, 0, 0), min = c(0L, 0L, 0L), hour = c(0L, 0L, 0L), mday = c(1L, 1L, 1L), mon = 1:3, year = c(101L, 101L, 101L), wday = c(4L, 4L, 0L), yday = c(31L, 59L, 90L), isdst = c(0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"UTC\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(11354, 11382, 11413), class = \"Date\"))"));  
.Internal(`Date2POSIXlt`(argv[[1]]));  
}, o=expected);  

