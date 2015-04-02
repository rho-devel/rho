expected <- TRUE    
test(id=1, code={    
argv <- structure(list(x = "`", value = TRUE), .Names = c("x", "value"    
))    
do.call('assign', argv);    
},  o = expected);    
    
