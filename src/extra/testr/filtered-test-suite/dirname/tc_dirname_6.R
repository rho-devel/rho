expected <- character(0)    
test(id=0, code={    
argv <- structure(list(path = character(0)), .Names = "path")    
do.call('dirname', argv);    
},  o = expected);    
    
