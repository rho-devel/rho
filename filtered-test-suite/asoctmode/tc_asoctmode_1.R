expected <- structure(1:255, class = "octmode")    
test(id=0, code={    
argv <- structure(list(x = 1:255), .Names = "x")    
do.call('as.octmode', argv);    
},  o = expected);    
    
