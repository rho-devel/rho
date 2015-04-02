expected <- eval(parse(text="5L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(-1, 0, 1, 2, 3), .Tsp = c(-1, 3, 1)))"));          
do.call(`length`, argv);          
}, o=expected);          

