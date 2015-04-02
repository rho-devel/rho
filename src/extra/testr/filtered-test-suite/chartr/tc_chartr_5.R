expected <- c("na", NA, "Banana")    
test(id=0, code={    
argv <- structure(list(old = "NA", new = "na", x = c("NA", NA, "BANANA"    
)), .Names = c("old", "new", "x"))    
do.call('chartr', argv);    
},  o = expected);    
    
