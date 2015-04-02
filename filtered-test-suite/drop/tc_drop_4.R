expected <- c(8, 4, 2)     
test(id=1, code={     
argv <- structure(list(x = structure(c(8, 4, 2), .Dim = c(3L, 1L))), .Names = "x")     
do.call('drop', argv);     
},  o = expected);     
     
