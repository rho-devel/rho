expected <- eval(parse(text="structure(16146, class = \"Date\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = 33.1798663139343, min = 47L, hour = 14L, mday = 17L, mon = 2L, year = 114L, wday = 1L, yday = 75L, isdst = 1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")))"));  
.Internal(`POSIXlt2Date`(argv[[1]]));  
}, o=expected);  

