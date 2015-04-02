expected <- structure(1:3, id = "An Example")    
test(id=0, code={    
argv <- list(structure(1:3, id = "An Example", class = structure("numWithId", package = ".GlobalEnv")))    
do.call('xtfrm', argv);    
},  o = expected);    
    
