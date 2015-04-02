expected <- structure(integer(0), .Dim = c(5L, 0L))    
test(id=0, code={    
argv <- structure(list(x = structure(integer(0), .Dim = c(5L, 0L)), MARGIN = 2,     
    STATS = integer(0)), .Names = c("x", "MARGIN", "STATS"))    
do.call('sweep', argv);    
},  o = expected);    
    
