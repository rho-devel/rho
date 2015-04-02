expected <- character(0)    
test(id=0, code={    
argv <- structure(list(x = structure(integer(0), class = "AsIs")), .Names = "x")    
do.call('format.AsIs', argv);    
},  o = expected);    
    
