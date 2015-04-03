expected <- eval(parse(text="c(4L, 6L, 9L, 15L, NA)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(4L, 6L, 9L, 15L, NA), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

