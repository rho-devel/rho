expected <- eval(parse(text="list(NULL)"));              
test(id=0, code={              
argv <- eval(parse(text="list(list(NULL, NULL, NULL, NULL, NULL), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

