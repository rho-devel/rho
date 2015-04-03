expected <- structure(1024891210, class = c("POSIXct", "POSIXt"), tzone = "")     
test(id=1, code={     
argv <- structure(list(year = 2002, month = 6, day = 24, hour = 0, min = 0,      
    sec = 10), .Names = c("year", "month", "day", "hour", "min",      
"sec"))     
do.call('ISOdatetime', argv);     
},  o = expected);     
     
