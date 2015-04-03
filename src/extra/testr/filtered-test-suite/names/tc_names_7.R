expected <- eval(parse(text="NULL"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(1:20, .Tsp = c(1, 20, 1), class = \"ts\"))"));         
do.call(`names`, argv);         
}, o=expected);         

