test(id=4, code={     
argv <- structure(list(to = NaN), .Names = "to")     
do.call('seq.int', argv);     
}, e = "'to' cannot be NA, NaN or infinite");     
     
