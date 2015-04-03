expected <- structure(0, class = c("POSIXct", "POSIXt"), tzone = "GMT")    
test(id=0, code={    
argv <- structure(list(year = 1970, month = 1, day = 1, hour = 0, min = 0,     
    sec = 0, tz = "GMT"), .Names = c("year", "month", "day",     
"hour", "min", "sec", "tz"))    
do.call('ISOdatetime', argv);    
},  o = expected);    
    
