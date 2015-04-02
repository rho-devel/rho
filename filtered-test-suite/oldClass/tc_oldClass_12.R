expected <- eval(parse(text="\"ts\""));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(1:5, .Tsp = c(1, 5, 1), class = \"ts\"))"));      
do.call(`oldClass`, argv);      
}, o=expected);      

