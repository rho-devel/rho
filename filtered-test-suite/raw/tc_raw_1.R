expected <- raw(0)     
test(id=1, code={     
argv <- structure(list(length = 0), .Names = "length")     
do.call('raw', argv);     
},  o = expected);     
     
