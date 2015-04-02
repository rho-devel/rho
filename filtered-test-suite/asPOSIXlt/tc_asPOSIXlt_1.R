expected <- eval(parse(text="structure(list(sec = c(8.40000009536743, 8.80000019073486), min = c(14L, 14L), hour = c(22L, 22L), mday = c(18L, 18L), mon = c(0L, 0L), year = c(138L, 138L), wday = c(2L, 2L), yday = c(17L, 17L), isdst = c(0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(2147483648.4, 2147483648.8), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), \"\")"));  
.Internal(`as.POSIXlt`(argv[[1]], argv[[2]]));  
}, o=expected);  

