expected <- structure(2456940, origin = -2440588)    
test(id=0, code={    
argv <- structure(list(x = structure(16352, class = "Date"), origin = -2440588), .Names = c("x",     
"origin"))    
do.call('julian.Date', argv);    
},  o = expected);    
    
