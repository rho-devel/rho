expected <- c(-2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4)    
test(id=0, code={    
argv <- list(c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))    
do.call('ceiling', argv);    
},  o = expected);    
    
