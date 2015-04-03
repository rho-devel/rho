expected <- structure(c(1L, 1L), .Label = c("a.b.c", "a.b.b.c", "a.c"), class = "factor")    
test(id=0, code={    
argv <- list(c("a.b", "a"), c("c", "b.c"))    
do.call('interaction', argv);    
},  o = expected);    
    
