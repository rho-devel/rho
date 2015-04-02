expected <- structure(8.33333333333333, units = "mins", .Names = "a", class = "difftime")    
test(id=0, code={    
argv <- structure(list(x = structure(500, units = "secs", class = "difftime", .Names = "a"),     
    value = "mins"), .Names = c("x", "value"))    
do.call('units<-.difftime', argv);    
},  o = expected);    
    
