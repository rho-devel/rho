expected <- character(0)    
test(id=0, code={    
argv <- structure(list(x = character(0)), .Names = "x")    
do.call('sQuote', argv);    
},  o = expected);    
    
