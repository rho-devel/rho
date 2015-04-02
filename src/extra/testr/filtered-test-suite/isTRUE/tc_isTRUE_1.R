expected <- TRUE     
test(id=0, code={     
argv <- structure(list(x = TRUE), .Names = "x")     
do.call('isTRUE', argv);     
},  o = expected);     
     
