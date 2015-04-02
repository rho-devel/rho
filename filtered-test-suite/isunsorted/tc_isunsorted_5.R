expected <- TRUE     
test(id=0, code={     
argv <- structure(list(x = structure(list(x = 3:4, y = 1:2), .Names = c("x",      
"y"), row.names = c(NA, -2L), class = "data.frame")), .Names = "x")     
do.call('is.unsorted', argv);     
},  o = expected);     
     
