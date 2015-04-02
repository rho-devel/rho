expected <- eval(parse(text="3L"));      
test(id=0, code={      
argv <- list();      
do.call(`nargs`, argv);      
}, o=expected);      

