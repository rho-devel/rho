expected <- TRUE    
test(id=1, code={    
argv <- structure(list(target = structure(c("A", "E", "I", "M", "Q",     
"U", "B", "F", "J", "N", "R", "V", "C", "G", "K", "O", "S", "W",     
"D", "H", "L", "P", "T", "X"), .Dim = c(6L, 4L)), current = structure(c("A",     
"E", "I", "M", "Q", "U", "B", "F", "J", "N", "R", "V", "C", "G",     
"K", "O", "S", "W", "D", "H", "L", "P", "T", "X"), .Dim = c(6L,     
4L))), .Names = c("target", "current"))    
do.call('all.equal.character', argv);    
},  o = expected);    
    
