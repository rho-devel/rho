expected <- TRUE    
test(id=0, code={    
argv <- structure(list(x = 3), .Names = "x")    
do.call('is.vector', argv);    
},  o = expected);    
    
