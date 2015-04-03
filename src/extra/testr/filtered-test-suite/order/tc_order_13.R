expected <- 1L     
test(id=0, code={     
argv <- structure(list(1, 1, 1, na.last = NA), .Names = c("", "", "",      
"na.last"))     
do.call('order', argv);     
},  o = expected);     
     
