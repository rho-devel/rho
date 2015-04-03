expected <- c(0L, 0L)    
test(id=5, code={    
argv <- structure(list(length = 2), .Names = "length")    
do.call('integer', argv);    
},  o = expected);    
    
