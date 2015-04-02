expected <- 94.4520423659935     
test(id=1, code={     
argv <- structure(list(a = 0.01, b = 171), .Names = c("a", "b"))     
do.call('beta', argv);     
},  o = expected);     
     
