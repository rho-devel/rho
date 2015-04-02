expected <- TRUE     
test(id=1, code={     
argv <- list(expression(quote(expression(b = pi^3))))     
do.call('is.expression', argv);     
},  o = expected);     
     
