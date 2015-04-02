expected <- eval(parse(text="1:20"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:20, .Tsp = c(1, 20, 1), class = \"ts\"), \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

