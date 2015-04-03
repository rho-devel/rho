expected <- TRUE    
test(id=0, code={    
argv <- list(NULL)    
do.call('is.pairlist', argv);    
},  o = expected);    
    
