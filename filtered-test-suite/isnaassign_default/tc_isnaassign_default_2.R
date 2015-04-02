expected <- structure(c("A", NA, "C"), class = "AsIs")    
test(id=2, code={    
argv <- structure(list(x = structure(c("A", "3", "C"), class = "AsIs"),     
    value = 2), .Names = c("x", "value"))    
do.call('is.na<-.default', argv);    
},  o = expected);    
    
