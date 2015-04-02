expected <- eval(parse(text="c(\"1e-08\", \"25\", \"FALSE\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(epsilon = 1e-08, maxit = 25, trace = FALSE))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

