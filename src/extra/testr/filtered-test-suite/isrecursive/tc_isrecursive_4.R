expected <- TRUE    
test(id=0, code={    
argv <- list(expression(quote(expression(4, 1.12837916709551))))    
do.call('is.recursive', argv);    
},  o = expected);    
    
