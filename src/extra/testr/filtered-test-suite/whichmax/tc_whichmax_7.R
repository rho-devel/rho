expected <- integer(0)     
test(id=0, code={     
argv <- structure(list(x = c(NA, NA)), .Names = "x")     
do.call('which.max', argv);     
},  o = expected);     
     
