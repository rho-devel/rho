expected <- structure(c(2L, 5L), .Dim = 1:2)    
test(id=0, code={    
argv <- structure(list(x = structure(c(2L, 5L), .Dim = 1:2, class = "AsIs")), .Names = "x")    
do.call('print.AsIs', argv);    
},  o = expected);    
    
