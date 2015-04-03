expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 1L, mon = 0L, year = 70L, wday = 4L, yday = 0L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"))"));        
do.call(`is.na`, argv);        
}, o=expected);        

