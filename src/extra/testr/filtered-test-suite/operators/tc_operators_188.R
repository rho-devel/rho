expected <- eval(parse(text="1"));      
test(id=0, code={      
argv <- eval(parse(text="list(5, 3)"));      
do.call(`%/%`, argv);      
}, o=expected);      

