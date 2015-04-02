expected <- eval(parse(text="structure(c(13823, NA), class = \"Date\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = c(0, NA), min = c(0L, NA), hour = c(0L, NA), mday = c(6L, NA), mon = c(10L, NA), year = c(107L, NA), wday = c(2L, NA), yday = c(309L, NA), isdst = c(0L, -1L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\")))"));  
.Internal(`POSIXlt2Date`(argv[[1]]));  
}, o=expected);  

