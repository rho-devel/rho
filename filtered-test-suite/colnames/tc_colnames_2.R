expected <- c("x", "CC", "f")     
test(id=5, code={     
argv <- structure(list(x = structure(list(x = 1:6, CC = 11:16, f = structure(c(1L,      
1L, 2L, 2L, 3L, 3L), .Label = c("1", "2", "3"), class = "factor")), .Names = c("x",      
"CC", "f"), row.names = c(NA, -6L), class = "data.frame")), .Names = "x")     
do.call('colnames', argv);     
},  o = expected);     
     
