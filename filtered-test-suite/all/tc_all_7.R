expected <- eval(parse(text="FALSE"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, FALSE), .Tsp = c(-1, 3, 1), class = \"ts\"))"));     
do.call(`all`, argv);     
}, o=expected);     

