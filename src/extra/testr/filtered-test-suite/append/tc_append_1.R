expected <- c(1L, 2L, 3L, 0L, 1L, 4L, 5L)     
test(id=1, code={     
argv <- structure(list(x = 1:5, values = 0:1, after = 3), .Names = c("x",      
"values", "after"))     
do.call('append', argv);     
},  o = expected);     
     
