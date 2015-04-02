expected <- structure(NA, .Dim = c(1L, 1L))    
test(id=0, code={    
argv <- structure(list(object = structure(NA, .Dim = c(1L, 1L))), .Names = "object")    
do.call('asS4', argv);    
},  o = expected);    
    
