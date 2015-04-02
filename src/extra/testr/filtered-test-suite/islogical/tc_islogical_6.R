expected <- TRUE    
test(id=1, code={    
argv <- list(c(FALSE, TRUE, FALSE))    
do.call('is.logical', argv);    
},  o = expected);    
    
