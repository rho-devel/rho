expected <- eval(parse(text="0"));      
test(id=0, code={      
argv <- eval(parse(text="list(1, 1)"));      
do.call(`-`, argv);      
}, o=expected);      

