expected <- "temp"    
test(id=0, code={    
argv <- structure(list(expr = expression(quote(temp[1, ] ~ 3))), .Names = "expr")    
do.call('all.vars', argv);    
},  o = expected);    
    
