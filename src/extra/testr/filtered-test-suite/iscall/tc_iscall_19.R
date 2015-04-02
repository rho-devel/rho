expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(3.14159265358979, .Tsp = c(1, 1, 1), class = \"ts\"))"));              
do.call(`is.call`, argv);              
}, o=expected);              

