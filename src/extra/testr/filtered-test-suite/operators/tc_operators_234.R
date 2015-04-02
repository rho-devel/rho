expected <- eval(parse(text="structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Tsp = c(2, 10, 1), class = \"ts\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(2:10, .Tsp = c(2, 10, 1), class = \"ts\"), structure(1:9, .Tsp = c(2, 10, 1), class = \"ts\"))"));      
do.call(`-`, argv);      
}, o=expected);      

