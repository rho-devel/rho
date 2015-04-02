expected <- "/tmp/RtmpagC9oa/Pkgs/exNSS4"     
test(id=13, code={     
argv <- structure(list(path = "/tmp/RtmpagC9oa/Pkgs/exNSS4"), .Names = "path")     
do.call('path.expand', argv);     
},  o = expected);     
     
