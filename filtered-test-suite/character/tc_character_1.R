expected <- character(0)    
test(id=0, code={    
argv <- structure(list(length = 0L), .Names = "length")    
do.call('character', argv);    
},  o = expected);    
    
