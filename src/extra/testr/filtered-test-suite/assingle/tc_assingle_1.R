expected <- structure(1, Csingle = TRUE)    
test(id=0, code={    
argv <- structure(list(x = 1), .Names = "x")    
do.call('as.single', argv);    
},  o = expected);    
    
