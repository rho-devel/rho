expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1L, 1L, 1L, 1L, 1L))"));      
do.call(`any`, argv);      
}, o=expected);      

