expected <- TRUE       
test(id=5, code={       
argv <- structure(list(target = structure(1412833061.16639, class = c("POSIXct",        
"POSIXt")), current = structure(list(sec = 41.1663863658905,        
    min = 37L, hour = 1L, mday = 9L, mon = 9L, year = 114L, wday = 4L,        
    yday = 281L, isdst = 1L, zone = "EDT", gmtoff = -14400L), .Names = c("sec",        
"min", "hour", "mday", "mon", "year", "wday", "yday", "isdst",        
"zone", "gmtoff"), class = c("POSIXlt", "POSIXt"), tzone = c("",        
"EST", "EDT"))), .Names = c("target", "current"))       
do.call('all.equal.POSIXt', argv);       
},  o = expected);       
       
