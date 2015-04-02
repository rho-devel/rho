expected <- eval(parse(text="FALSE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(sec = 40, min = 24L, hour = 11L, mday = 15L, mon = 11L, year = 100L, wday = 5L, yday = 349L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\")))"));                
do.call(`is.na`, argv);                
}, o=expected);                

