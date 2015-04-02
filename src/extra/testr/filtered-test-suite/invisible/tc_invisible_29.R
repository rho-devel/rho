expected <- eval(parse(text="structure(NA, .Tsp = c(1, 1, 1), class = \"ts\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(NA, .Tsp = c(1, 1, 1), class = \"ts\"))"));             
do.call(`invisible`, argv);             
}, o=expected);             

