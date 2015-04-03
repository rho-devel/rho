expected <- TRUE    
test(id=2, code={    
argv <- structure(list(TZ = "EST5EDT"), .Names = "TZ")    
do.call('Sys.setenv', argv);    
},  o = expected);    
    
