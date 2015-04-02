expected <- 2L    
test(id=1, code={    
argv <- structure(list(x = structure(c(1L, 2L, NA), .Label = c("1", "2"    
), class = "factor")), .Names = "x")    
do.call('nlevels', argv);    
},  o = expected);    
    
