expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1, 1, NA, 2))"));      
do.call(`is.numeric`, argv);      
}, o=expected);      

