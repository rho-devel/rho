expected <- eval(parse(text="structure(list(sec = c(0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L), hour = c(12L, 12L, 12L, 12L, 12L), mday = 22:26, mon = c(3L, 3L, 3L, 3L, 3L), year = c(108L, 108L, 108L, 108L, 108L), wday = 2:6, yday = 112:116, isdst = c(0L, 0L, 0L, 0L, 0L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(1208865600, 1208952000, 1209038400, 1209124800, 1209211200), tzone = \"GMT\", class = c(\"POSIXct\", \"POSIXt\")), \"GMT\")"));  
.Internal(`as.POSIXlt`(argv[[1]], argv[[2]]));  
}, o=expected);  

