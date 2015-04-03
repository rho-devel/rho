expected <- c("row1", "row2", "row3", "row4")    
test(id=3, code={    
argv <- structure(list(x = structure(logical(0), .Dim = c(4L, 0L)), do.NULL = FALSE), .Names = c("x",     
"do.NULL"))    
do.call('rownames', argv);    
},  o = expected);    
    
