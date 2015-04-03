expected <- numeric(0)     
test(id=0, code={     
argv <- structure(list(x = structure(numeric(0), .Dim = c(0L, 2L))), .Names = "x")     
do.call('rowSums', argv);     
},  o = expected);     
     
