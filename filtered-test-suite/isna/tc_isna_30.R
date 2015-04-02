expected <- eval(parse(text="FALSE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 12L, mon = 2L, year = 112L, wday = 1L, yday = 71L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

