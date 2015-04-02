expected <- "Mean absolute difference: 0.002000093"     
test(id=6, code={     
argv <- structure(list(target = structure(1412833061.16639, class = c("POSIXct",      
"POSIXt")), current = structure(1412833061.16839, class = c("POSIXct",      
"POSIXt"))), .Names = c("target", "current"))     
do.call('all.equal.POSIXt', argv);     
},  o = expected);     
     
