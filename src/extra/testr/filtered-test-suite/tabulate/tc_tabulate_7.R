expected <- 0L    
test(id=0, code={    
argv <- structure(list(bin = numeric(0)), .Names = "bin")    
do.call('tabulate', argv);    
},  o = expected);    
    
