expected <- eval(parse(text="list(FALSE)"));              
test(id=0, code={              
argv <- eval(parse(text="list(list(FALSE), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

