expected <- 0L    
test(id=4, code={    
argv <- structure(list(x = c(1, NA, 3, NA, 3), incomparables = c(3, NA    
)), .Names = c("x", "incomparables"))    
do.call('anyDuplicated.default', argv);    
},  o = expected);    
    
