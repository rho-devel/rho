expected <- structure(c(1, 2), .Dim = 2L, .Dimnames = list(c("a", "b")))    
test(id=0, code={    
argv <- structure(list(x = structure(c(1, 2), .Dim = 2L, .Dimnames = list(    
    c("a", "b")))), .Names = "x")    
do.call('as.array.default', argv);    
},  o = expected);    
    
