expected <- structure(numeric(0), .Dim = c(0L, 4L))    
test(id=0, code={    
argv <- structure(list(x = structure(numeric(0), .Dim = c(0L, 4L)), value = numeric(0)), .Names = c("x",     
"value"))    
do.call('diag<-', argv);    
},  o = expected);    
    
