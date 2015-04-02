expected <- logical(0)    
test(id=0, code={    
argv <- structure(list(length = 0), .Names = "length")    
do.call('logical', argv);    
},  o = expected);    
    
