expected <- eval(parse(text="structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9), .Tsp = c(1960.08333333333, 1961.66666666667, 12), class = \"ts\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(1, structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8), .Tsp = c(1960.08333333333, 1961.66666666667, 12), class = \"ts\"))"));    
do.call(`+`, argv);    
}, o=expected);    

