expected <- structure(3L, .Label = c("A", "B", "C", "D", "E"), class = "factor")    
test(id=0, code={    
argv <- structure(list(x = structure(2:4, .Label = c("A", "B", "C", "D",     
"E"), class = "factor"), 2), .Names = c("x", ""))    
do.call('[[.factor', argv);    
},  o = expected);    
    
