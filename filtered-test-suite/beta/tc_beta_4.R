expected <- 2e+200     
test(id=0, code={     
argv <- structure(list(a = 1e-200, b = 1e-200), .Names = c("a", "b"))     
do.call('beta', argv);     
},  o = expected);     
     
