expected <- eval(parse(text="structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Tsp = c(1920.5, 1921.25, 12), class = \"ts\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(1:10, .Tsp = c(1920.5, 1921.25, 12), class = \"ts\"), 1)"));    
do.call(`+`, argv);    
}, o=expected);    

