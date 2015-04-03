expected <- structure(8, .Names = "a")    
test(id=1, code={    
argv <- structure(list(x = structure(list(a = 6:10), .Names = "a", row.names = 6:10, class = "data.frame")), .Names = "x")    
do.call('colMeans', argv);    
},  o = expected);    
    
