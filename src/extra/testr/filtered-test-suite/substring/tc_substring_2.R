expected <- c("a", "b", "c", "d", "e", "f")    
test(id=2, code={    
argv <- structure(list(text = "abcdef", first = 1:6, last = 1:6), .Names = c("text",     
"first", "last"))    
do.call('substring', argv);    
},  o = expected);    
    
