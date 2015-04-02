expected <- eval(parse(text="c(TRUE, TRUE)"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(30000L, 100000L), c(30000, 1e+05))"));            
do.call(`==`, argv);            
}, o=expected);            

