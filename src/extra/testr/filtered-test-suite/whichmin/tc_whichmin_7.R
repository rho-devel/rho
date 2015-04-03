expected <- 3L     
test(id=1, code={     
argv <- structure(list(x = c(NA, NA, Inf)), .Names = "x")     
do.call('which.min', argv);     
},  o = expected);     
     
