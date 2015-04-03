expected <- eval(parse(text="c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(c(TRUE, TRUE), c(TRUE, TRUE), c(TRUE, TRUE), c(TRUE, TRUE), c(1, 2, 3)), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

