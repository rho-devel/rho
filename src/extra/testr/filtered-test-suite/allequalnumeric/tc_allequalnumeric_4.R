expected <- TRUE             
test(id=2818, code={             
argv <- structure(list(target = 3.18309886183776e-301, current = 3.18309886183791e-301,              
    tolerance = 1e-15), .Names = c("target", "current", "tolerance"             
))             
do.call('all.equal.numeric', argv);             
},  o = expected);             
             
