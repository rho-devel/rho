expected <- eval(parse(text="c(-1.6, -0.9)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(-1.6, -0.9))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

