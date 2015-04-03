expected <- structure(integer(0), class = "octmode")    
test(id=0, code={    
argv <- structure(list(a = structure(integer(0), class = "octmode"),     
    b = "400"), .Names = c("a", "b"))    
do.call('&.octmode', argv);    
},  o = expected);    
    
