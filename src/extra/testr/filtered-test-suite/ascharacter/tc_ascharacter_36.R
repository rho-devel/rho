expected <- eval(parse(text="c(\"0\", \"NULL\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(exit.code = 0L, send = NULL))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

