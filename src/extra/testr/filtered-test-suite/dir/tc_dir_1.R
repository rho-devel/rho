expected <- character(0)    
test(id=1, code={    
argv <- structure(list(path = ".", pattern = "myTst_.*tar\\.gz$"), .Names = c("path",     
"pattern"))    
do.call('dir', argv);    
},  o = expected);    
    
