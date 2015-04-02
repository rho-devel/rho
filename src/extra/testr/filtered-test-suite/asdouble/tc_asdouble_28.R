expected <- eval(parse(text="c(3, 4, 5)"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(3:5, .Tsp = c(1, 3, 1), class = \"ts\"))"));     
do.call(`as.double`, argv);     
}, o=expected);     

