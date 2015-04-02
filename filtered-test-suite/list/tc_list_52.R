expected <- eval(parse(text="list(structure(list(sec = 54.5054557323456, min = 53L, hour = 23L, mday = 6L, mon = 11L, year = 113L, wday = 5L, yday = 339L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(sec = 54.5054557323456, min = 53L, hour = 23L, mday = 6L, mon = 11L, year = 113L, wday = 5L, yday = 339L, isdst = 0L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = c(\"\", \"EST\", \"EDT\")))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

