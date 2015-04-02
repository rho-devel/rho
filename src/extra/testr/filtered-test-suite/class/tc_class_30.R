expected <- eval(parse(text="\"ts\""));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(1:10, .Tsp = c(1959.25, 1961.5, 4), class = \"ts\"))"));              
do.call(`class`, argv);              
}, o=expected);              

