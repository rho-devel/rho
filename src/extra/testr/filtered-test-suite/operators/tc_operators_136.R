expected <- eval(parse(text="structure(c(TRUE, TRUE, FALSE, TRUE, TRUE), .Tsp = c(-1, 3, 1), class = \"ts\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(2, 1, 0, 1, 2), .Tsp = c(-1, 3, 1), class = \"ts\"), 1e-05)"));           
do.call(`>`, argv);           
}, o=expected);           

