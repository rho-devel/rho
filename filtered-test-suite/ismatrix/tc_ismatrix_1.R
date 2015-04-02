expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(-3.44, 62.44))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

