expected <- TRUE    
test(id=0, code={    
argv <- structure(list(x = ".Device"), .Names = "x")    
do.call('exists', argv);    
},  o = expected);    
    
