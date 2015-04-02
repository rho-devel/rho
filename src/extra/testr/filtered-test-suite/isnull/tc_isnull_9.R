expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Tsp = c(1920.5, 1921.25, 12), class = \"ts\"))"));   
do.call(`is.null`, argv);   
}, o=expected);   

