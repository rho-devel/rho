expected <- structure(logical(0), .Dim = 0:1, .Dimnames = list(NULL, "A"))     
test(id=0, code={     
argv <- structure(list(x = structure(logical(0), .Dim = 0:1)), .Names = "x")     
do.call('provideDimnames', argv);     
},  o = expected);     
     
