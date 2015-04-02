expected <- c("NA", NA, "BANANA")    
test(id=0, code={    
argv <- structure(list(x = c("na", NA, "banana")), .Names = "x")    
do.call('toupper', argv);    
},  o = expected);    
    
