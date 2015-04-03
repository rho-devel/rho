expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(sec = 0, min = 0L, hour = 0L, mday = 9L, mon = 9L, year = 103L, wday = 4L, yday = 281L, isdst = 1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\")))"));        
do.call(`is.na`, argv);        
}, o=expected);        

