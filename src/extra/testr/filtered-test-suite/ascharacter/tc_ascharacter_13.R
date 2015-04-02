expected <- eval(parse(text="\"1\""));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(\"1\", .Tsp = c(1, 1, 1), class = \"ts\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

