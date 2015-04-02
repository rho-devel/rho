expected <- c("1", "2", "3")     
test(id=3, code={     
argv <- structure(list(x = structure(list(x = 1:3, y = c(6.28318530717959,      
3.14159265358979, 0)), .Names = c("x", "y"), row.names = c(NA,      
-3L), class = "data.frame")), .Names = "x")     
do.call('row.names', argv);     
},  o = expected);     
     
