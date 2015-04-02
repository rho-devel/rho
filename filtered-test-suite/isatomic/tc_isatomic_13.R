expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(1, 0, 2, NA, 3))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

