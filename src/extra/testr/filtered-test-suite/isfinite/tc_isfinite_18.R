expected <- eval(parse(text="TRUE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(1, .Tsp = c(1, 1, 1), class = \"ts\"))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

