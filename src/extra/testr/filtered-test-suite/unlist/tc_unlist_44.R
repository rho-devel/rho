expected <- eval(parse(text="c(0, 0, 0, 0, 0, 1, 1, 1)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(c(0, 0), c(0, 0, 0, 1), NULL, c(1, 1)), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

