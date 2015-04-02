expected <- structure(c(13823, NA), class = "Date")        
test(id=12, code={        
argv <- structure(list(x = c("2007-11-06", NA)), .Names = "x")        
do.call('as.Date.character', argv);        
},  o = expected);        
        
