expected <- eval(parse(text="c(3, 4, 5, 8, 9, 10, 11, 12, 13)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(3, 4, 5, 11, 10, 9, 8, 8, 9, 10, 11, 12, 13), FALSE, TRUE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

