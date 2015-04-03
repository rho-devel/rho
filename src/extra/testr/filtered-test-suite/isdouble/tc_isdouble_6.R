expected <- TRUE    
test(id=0, code={    
argv <- list(structure(c(1, 5, 9, 13, 17, 21, 2, 6, 10, 14, 18, 22, 3,     
7, 11, 15, 19, 23, 4, 8, 12, 16, 20, 24), .Dim = c(6L, 4L)))    
do.call('is.double', argv);    
},  o = expected);    
    
