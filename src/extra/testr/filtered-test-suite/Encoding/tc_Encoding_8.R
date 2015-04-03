expected <- "unknown"    
test(id=0, code={    
argv <- structure(list(x = "abc"), .Names = "x")    
do.call('Encoding', argv);    
},  o = expected);    
    
