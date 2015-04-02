expected <- 1L       
test(id=1, code={       
argv <- structure(list(1, na.last = NA), .Names = c("", "na.last"))       
do.call('order', argv);       
},  o = expected);       
       
