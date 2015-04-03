expected <- eval(parse(text="structure(list(sec = 59.7693939208984, min = 47L, hour = 18L, mday = 17L, mon = 2L, year = 114L, wday = 1L, yday = 75L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(sec = 59.7693939208984, min = 47L, hour = 18L, mday = 17L, mon = 2L, year = 114L, wday = 1L, yday = 75L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"))"));      
do.call(`invisible`, argv);      
}, o=expected);      

