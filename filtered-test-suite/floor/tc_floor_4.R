expected <- eval(parse(text="structure(c(1920, 1920, 1920, 1920, 1920, 1920, 1921, 1921, 1921, 1921), .Tsp = c(1920.5, 1921.25, 12), class = \"ts\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(1920.5, 1920.5833, 1920.6667, 1920.75, 1920.8333, 1920.9167, 1921, 1921.0833, 1921.1667, 1921.25), .Tsp = c(1920.5, 1921.25, 12), class = \"ts\"))"));    
do.call(`floor`, argv);    
}, o=expected);    

