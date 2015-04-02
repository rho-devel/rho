expected <- FALSE     
test(id=1, code={     
argv <- structure(list(x = structure(list(sec = 0, min = 0L, hour = 0L,      
    mday = 11L, mon = 7L, year = 3L, wday = 2L, yday = 222L,      
    isdst = 0L, zone = "EST", gmtoff = NA_integer_), .Names = c("sec",      
"min", "hour", "mday", "mon", "year", "wday", "yday", "isdst",      
"zone", "gmtoff"), class = c("POSIXlt", "POSIXt"), tzone = c("EST5EDT",      
"EST", "EDT"))), .Names = "x")     
do.call('is.na.POSIXlt', argv);     
},  o = expected);     
     
