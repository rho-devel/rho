expected <- eval(parse(text="c(0.9, 0.1, 0.3, 0.5, 0.7, 0.9, 0.1, 0.3, 0.5)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(0.9, 0.1, 0.3, 0.5, 0.7, 0.9, 0.1, 0.3, 0.5))"));      
do.call(`abs`, argv);      
}, o=expected);      

