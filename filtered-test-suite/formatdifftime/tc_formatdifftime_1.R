expected <- c(" 0 mins", "30 mins", "60 mins")    
test(id=0, code={    
argv <- structure(list(x = structure(c(0, 30, 60), units = "mins", class = "difftime")), .Names = "x")    
do.call('format.difftime', argv);    
},  o = expected);    
    
