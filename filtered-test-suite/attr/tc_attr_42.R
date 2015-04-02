expected <- eval(parse(text="c(1920.5, 1921.25, 12)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Dim = c(10L, 2L), .Dimnames = list(NULL, c(\"tt\", \"tt + 1\")), .Tsp = c(1920.5, 1921.25, 12), class = c(\"mts\", \"ts\", \"matrix\")), \"tsp\")"));        
do.call(`attr`, argv);        
}, o=expected);        

