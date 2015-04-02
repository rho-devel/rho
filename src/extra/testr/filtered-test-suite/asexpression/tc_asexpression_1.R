expected <- expression(1)    
test(id=0, code={    
argv <- structure(list(x = 1), .Names = "x")    
do.call('as.expression', argv);    
},  o = expected);    
    
