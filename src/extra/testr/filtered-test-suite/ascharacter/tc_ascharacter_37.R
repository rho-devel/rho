expected <- eval(parse(text="c(\"34\", \"-45\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(34L, -45L))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

