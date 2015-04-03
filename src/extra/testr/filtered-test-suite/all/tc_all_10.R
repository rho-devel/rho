expected <- eval(parse(text="FALSE"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, FALSE, TRUE, TRUE), .Tsp = c(1, 5, 1), class = \"ts\"))"));     
do.call(`all`, argv);     
}, o=expected);     

