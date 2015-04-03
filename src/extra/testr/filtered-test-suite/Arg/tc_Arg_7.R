expected <- 3.14159265358979    
test(id=0, code={    
argv <- list(-1)    
do.call('Arg', argv);    
},  o = expected);    
    
