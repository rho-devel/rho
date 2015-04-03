expected <- c("na", NA, "banana")    
test(id=0, code={    
argv <- structure(list(x = c("NA", NA, "BANANA")), .Names = "x")    
do.call('tolower', argv);    
},  o = expected);    
    
