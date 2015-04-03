expected <- structure(c(2L, 1L, 3L), .Label = c("One", "Two", "Three"), class = "factor")    
test(id=0, code={    
argv <- structure(list(x = structure(c(2L, 2L, 3L), .Label = c("One",     
"Two", "Three"), class = "factor"), 2, value = "One"), .Names = c("x",     
"", "value"))    
do.call('[[<-.factor', argv);    
},  o = expected);    
    
