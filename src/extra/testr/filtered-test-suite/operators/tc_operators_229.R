expected <- eval(parse(text="structure(c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), .Tsp = c(3, 10, 1), class = \"ts\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Tsp = c(3, 10, 1), class = \"ts\"), structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Tsp = c(3, 10, 1), class = \"ts\"))"));      
do.call(`-`, argv);      
}, o=expected);      

