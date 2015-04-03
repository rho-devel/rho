expected <- eval(parse(text="list(1)"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(a = 1), .Names = \"a\"), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

