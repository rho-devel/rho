expected <- 4L    
test(id=3, code={    
argv <- structure(list(x = structure(c(3, 2, 7, 2, 6, 2, 7, 2), .Dim = c(4L,     
2L), .Dimnames = list(c("A", "B", "C", "D"), c("M", "F"))), MARGIN = 0), .Names = c("x",     
"MARGIN"))    
do.call('anyDuplicated', argv);    
},  o = expected);    
    
