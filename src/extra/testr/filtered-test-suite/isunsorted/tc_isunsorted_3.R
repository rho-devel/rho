expected <- TRUE     
test(id=5, code={     
argv <- structure(list(x = c(NA, 1, 2, 3, 2), na.rm = TRUE), .Names = c("x",      
"na.rm"))     
do.call('is.unsorted', argv);     
},  o = expected);     
     
