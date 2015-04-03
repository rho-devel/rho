expected <- structure(list(Freq = integer(0)), .Names = "Freq", row.names = integer(0), class = "data.frame")    
test(id=0, code={    
argv <- structure(list(x = structure(integer(0), .Dim = 0L, .Dimnames = structure(list(    
    NULL), .Names = ""), class = "table")), .Names = "x")    
do.call('as.data.frame.table', argv);    
},  o = expected);    
    
