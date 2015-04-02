expected <- TRUE    
test(id=0, code={    
argv <- structure(list(x = structure(c(1L, 3L), .Label = c("b", "c",     
"a"), class = c("ordered", "factor"))), .Names = "x")    
do.call('is.ordered', argv);    
},  o = expected);    
    
