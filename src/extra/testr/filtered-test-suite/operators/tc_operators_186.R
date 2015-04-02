expected <- eval(parse(text="15"));      
test(id=0, code={      
argv <- eval(parse(text="list(243L, 16)"));      
do.call(`%/%`, argv);      
}, o=expected);      

