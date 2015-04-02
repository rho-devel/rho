expected <- eval(parse(text="structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\"))"));             
.Internal(drop(argv[[1]]));             
}, o=expected);             

