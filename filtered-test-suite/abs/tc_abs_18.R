expected <- eval(parse(text="structure(c(8, 7, 6, 5, 4, 3, 2, 1, 0, 1), .Tsp = c(2, 11, 1), class = \"ts\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(8, 7, 6, 5, 4, 3, 2, 1, 0, -1), .Tsp = c(2, 11, 1), class = \"ts\"))"));      
do.call(`abs`, argv);      
}, o=expected);      

