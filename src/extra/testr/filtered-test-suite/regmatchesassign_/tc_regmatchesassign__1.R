expected <- c("A", "B", "C")    
test(id=0, code={    
argv <- structure(list(x = c("A", "B", "C"), m = structure(c(1L, -1L,     
1L), match.length = c(1L, -1L, 1L), useBytes = TRUE), value = c("A",     
"C")), .Names = c("x", "m", "value"))    
do.call('regmatches<-', argv);    
},  o = expected);    
    
