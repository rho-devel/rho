expected <- structure(c(3.33333333333333, 683.25), units = "mins", class = "difftime")    
test(id=0, code={    
argv <- structure(list(tim = c("0:3:20", "11:23:15")), .Names = "tim")    
do.call('as.difftime', argv);    
},  o = expected);    
    
