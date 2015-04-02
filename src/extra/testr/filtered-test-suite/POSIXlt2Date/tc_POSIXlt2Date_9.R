expected <- eval(parse(text="structure(c(13991, 13992, 13993, 13994, 13995), class = \"Date\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(sec = c(0, 0, 0, 0, 0), min = c(0L, 0L, 0L, 0L, 0L), hour = c(0L, 0L, 0L, 0L, 0L), mday = 22:26, mon = c(3L, 3L, 3L, 3L, 3L), year = c(108L, 108L, 108L, 108L, 108L), wday = 2:6, yday = 112:116, isdst = c(-1L, -1L, -1L, -1L, -1L)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"))"));  
.Internal(`POSIXlt2Date`(argv[[1]]));  
}, o=expected);  

