expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1920, 1920, 1920, 1920, 1920, 1920, 1921, 1921, 1921, 1921), .Tsp = c(1920.5, 1921.25, 12), class = \"ts\"))"));       
do.call(`is.list`, argv);       
}, o=expected);       

