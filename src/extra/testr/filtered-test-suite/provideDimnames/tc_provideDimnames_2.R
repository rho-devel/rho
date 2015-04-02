expected <- structure(integer(0), .Dim = 0L, .Dimnames = structure(list(NULL), .Names = ""), class = "table")    
test(id=1, code={    
argv <- structure(list(x = structure(integer(0), .Dim = 0L, .Dimnames = structure(list(    
    NULL), .Names = ""), class = "table")), .Names = "x")    
do.call('provideDimnames', argv);    
},  o = expected);    
    
