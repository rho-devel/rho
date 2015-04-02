expected <- TRUE             
test(id=595, code={             
argv <- structure(list(target = -13.053274367453, current = -13.053274367453,              
    tolerance = 8e-16), .Names = c("target", "current", "tolerance"             
))             
do.call('all.equal.numeric', argv);             
},  o = expected);             
             
