expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(0, 0, 0, 0, 0, 0, 2.47032822920623e-323, 0, 0, 0, 0, 0))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

