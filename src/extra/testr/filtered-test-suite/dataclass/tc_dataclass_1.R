expected <- "character"    
test(id=0, code={    
argv <- structure(list(x = c("A", "B", "C", NA)), .Names = "x")    
do.call('data.class', argv);    
},  o = expected);    
    
