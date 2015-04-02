expected <- c(FALSE, TRUE)    
test(id=0, code={    
argv <- structure(list(x = structure(1:2, .Label = c("FALSE", "TRUE"), class = "factor")), .Names = "x")    
do.call('as.logical.factor', argv);    
},  o = expected);    
    
