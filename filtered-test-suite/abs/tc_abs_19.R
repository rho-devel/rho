expected <- eval(parse(text="32L"));      
test(id=0, code={      
argv <- eval(parse(text="list(-32L)"));      
do.call(`abs`, argv);      
}, o=expected);      

