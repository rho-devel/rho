expected <- 0   
test(id=0, code={   
argv <- structure(list(x = structure(c(FALSE, TRUE, FALSE, TRUE, TRUE,    
FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,    
TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE   
), .Dim = c(5L, 5L))), .Names = "x")   
do.call('rcond', argv);   
},  o = expected);   
   
