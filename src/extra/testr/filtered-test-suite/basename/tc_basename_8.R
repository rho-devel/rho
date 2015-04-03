expected <- "myTst"    
test(id=2, code={    
argv <- structure(list(path = "myTst"), .Names = "path")    
do.call('basename', argv);    
},  o = expected);    
    
