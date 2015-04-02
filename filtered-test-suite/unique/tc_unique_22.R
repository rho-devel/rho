expected <- eval(parse(text="c(25, 50, 100, 250, 500, 1e+05)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(25, 50, 100, 250, 500, 1e+05), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

