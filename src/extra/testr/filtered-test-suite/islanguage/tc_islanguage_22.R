expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(0:100, .Tsp = c(1, 101, 1), class = \"ts\"))"));    
do.call(`is.language`, argv);    
}, o=expected);    

