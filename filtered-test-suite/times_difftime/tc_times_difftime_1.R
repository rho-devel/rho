expected <- structure(c(6.66666666666667, 1366.5), units = "mins", class = "difftime")    
test(id=0, code={    
argv <- structure(list(e1 = 2, e2 = structure(c(3.33333333333333, 683.25    
), units = "mins", class = "difftime")), .Names = c("e1", "e2"    
))    
do.call('*.difftime', argv);    
},  o = expected);    
    
