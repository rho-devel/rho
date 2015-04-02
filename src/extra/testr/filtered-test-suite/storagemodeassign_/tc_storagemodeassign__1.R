expected <- structure(0:2, .Dim = c(3L, 1L))    
test(id=0, code={    
argv <- structure(list(structure(c(0, 1, 2), .Dim = c(3L, 1L)), value = "integer"), .Names = c("",     
"value"))    
do.call('storage.mode<-', argv);    
},  o = expected);    
    
