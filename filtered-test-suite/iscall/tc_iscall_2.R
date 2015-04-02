expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:10, .Tsp = c(1920.5, 1921.25, 12), class = \"ts\"))"));       
do.call(`is.call`, argv);       
}, o=expected);       

