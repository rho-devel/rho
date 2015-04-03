expected <- TRUE     
test(id=1, code={     
argv <- structure(list(target = as.raw(c(0x01, 0x02, 0x03)), current = as.raw(c(0x01,      
0x02, 0x03))), .Names = c("target", "current"))     
do.call('all.equal.raw', argv);     
},  o = expected);     
     
