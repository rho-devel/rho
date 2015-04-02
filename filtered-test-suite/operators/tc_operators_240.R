expected <- eval(parse(text="structure(c(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), .Tsp = c(1, 10, 1), class = \"ts\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(10, structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), .Tsp = c(1, 10, 1), class = \"ts\"))"));      
do.call(`-`, argv);      
}, o=expected);      

