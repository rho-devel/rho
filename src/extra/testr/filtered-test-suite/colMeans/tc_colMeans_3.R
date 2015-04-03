expected <- structure(3, .Names = "a")     
test(id=0, code={     
argv <- structure(list(x = structure(list(a = 1:5), .Names = "a", row.names = c(NA,      
5L), class = "data.frame")), .Names = "x")     
do.call('colMeans', argv);     
},  o = expected);     
     
