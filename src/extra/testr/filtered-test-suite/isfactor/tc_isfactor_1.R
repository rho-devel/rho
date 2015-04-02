expected <- FALSE     
test(id=4, code={     
argv <- structure(list(x = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,      
FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,      
FALSE, FALSE, FALSE)), .Names = "x")     
do.call('is.factor', argv);     
},  o = expected);     
     
