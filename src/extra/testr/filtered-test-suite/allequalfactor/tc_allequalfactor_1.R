expected <- TRUE    
test(id=0, code={    
argv <- structure(list(target = structure(c(4L, 5L, 1L, 5L, 3L, 4L, 5L,     
3L, 2L, 4L), .Label = c("a", "c", "i", "s", "t"), class = "factor", contrasts = structure(c(1,     
0, 0, 0, -1, 0, 1, 0, 0, -1, -0.247125681008604, -0.247125681008604,     
-0.149872105789645, 0.891249148815458, -0.247125681008604, 0.268816352031209,     
0.268816352031209, -0.881781351530059, 0.0753322954364324, 0.268816352031209    
), .Dim = c(5L, 4L), .Dimnames = list(c("a", "c", "i", "s", "t"    
), NULL))), current = structure(c(4L, 5L, 1L, 5L, 3L, 4L, 5L,     
3L, 2L, 4L), .Label = c("a", "c", "i", "s", "t"), class = "factor", contrasts = structure(c(1,     
0, 0, 0, -1, 0, 1, 0, 0, -1, -0.247125681008604, -0.247125681008604,     
-0.149872105789645, 0.891249148815458, -0.247125681008604, 0.268816352031209,     
0.268816352031209, -0.881781351530059, 0.0753322954364324, 0.268816352031209    
), .Dim = c(5L, 4L), .Dimnames = list(c("a", "c", "i", "s", "t"    
), NULL)))), .Names = c("target", "current"))    
do.call('all.equal.factor', argv);    
},  o = expected);    
    
