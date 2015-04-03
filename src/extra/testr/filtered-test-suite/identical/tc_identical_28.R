expected <- TRUE            
test(id=235, code={            
argv <- structure(list(x = expression(exp(-0.5 * u^2)), y = expression(            
    exp(-0.5 * u^2))), .Names = c("x", "y"))            
do.call('identical', argv);            
},  o = expected);            
            
