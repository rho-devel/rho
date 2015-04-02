expected <- eval(parse(text="2L"));      
test(id=0, code={      
argv <- eval(parse(text="list(4L, 2L)"));      
do.call(`%/%`, argv);      
}, o=expected);      

