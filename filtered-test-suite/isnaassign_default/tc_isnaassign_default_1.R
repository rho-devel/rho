expected <- NA_integer_    
test(id=0, code={    
argv <- structure(list(x = 9L, value = TRUE), .Names = c("x", "value"    
))    
do.call('is.na<-.default', argv);    
},  o = expected);    
    
