expected <- FALSE    
test(id=3, code={    
argv <- list(1:3)    
do.call('is.object', argv);    
},  o = expected);    
    
