expected <- eval(parse(text="structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Dim = c(10L, 2L), .Dimnames = list(NULL, c(\"tt\", \"tt + 1\")), .Tsp = c(1920.5, 1921.25, 12), class = c(\"mts\", \"ts\", \"matrix\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Dim = c(10L, 2L), .Dimnames = list(NULL, c(\"tt\", \"tt + 1\")), .Tsp = c(1920.5, 1921.25, 12), class = c(\"mts\", \"ts\", \"matrix\")))"));      
do.call(`invisible`, argv);      
}, o=expected);      

