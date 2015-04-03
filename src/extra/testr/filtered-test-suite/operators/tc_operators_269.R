expected <- eval(parse(text="structure(c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Tsp = c(3, 11, 1), class = \"ts\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8), .Tsp = c(3, 11, 1), class = \"ts\"), 1e-05)"));   
do.call(`>`, argv);   
}, o=expected);   

