expected <- structure(-0.5, class = "Date")    
test(id=4, code={    
argv <- structure(list(x = 0.5, origin = "1969-12-31"), .Names = c("x",     
"origin"))    
do.call('as.Date.numeric', argv);    
},  o = expected);    
    
