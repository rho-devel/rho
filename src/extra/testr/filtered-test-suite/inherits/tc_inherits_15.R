expected <- eval(parse(text="TRUE"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(FALSE, .Tsp = c(0, 0, 1), class = \"ts\"), \"ts\", FALSE)"));                   
.Internal(inherits(argv[[1]], argv[[2]], argv[[3]]));                   
}, o=expected);                   

