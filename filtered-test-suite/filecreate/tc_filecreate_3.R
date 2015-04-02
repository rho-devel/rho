expected <- TRUE    
test(id=0, code={    
argv <- structure(list("foo1"), .Names = "")    
do.call('file.create', argv);    
},  o = expected);    
    
