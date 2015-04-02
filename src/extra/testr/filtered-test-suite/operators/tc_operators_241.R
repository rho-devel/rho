expected <- eval(parse(text="7L"));      
test(id=0, code={      
argv <- eval(parse(text="list(17L, 10L)"));      
do.call(`-`, argv);      
}, o=expected);      

