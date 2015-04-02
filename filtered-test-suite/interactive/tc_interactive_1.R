expected <- FALSE    
test(id=0, code={    
argv <- list()    
do.call('interactive', argv);    
},  o = expected);    
    
