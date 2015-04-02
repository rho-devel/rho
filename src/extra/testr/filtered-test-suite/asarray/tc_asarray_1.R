expected <- structure(1:3, .Dim = 3L)     
test(id=0, code={     
argv <- structure(list(x = 1:3), .Names = "x")     
do.call('as.array', argv);     
},  o = expected);     
     
