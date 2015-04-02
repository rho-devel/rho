expected <- 0     
test(id=1, code={     
argv <- structure(list(x = structure(c(0, 0, 0, 0, 0, 0, NA, 0, 0, NA,      
NA, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))), .Names = "x")     
do.call('det', argv);     
},  o = expected);     
     
