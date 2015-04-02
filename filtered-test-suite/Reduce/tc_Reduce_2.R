expected <- 0       
test(id=1, code={       
argv <- structure(list(f = function (f, ...)        
f(...), x = list(.Primitive("log"), .Primitive("exp"), .Primitive("acos"),        
    .Primitive("cos")), init = 0, right = TRUE), .Names = c("f",        
"x", "init", "right"))       
do.call('Reduce', argv);       
},  o = expected);       
       
