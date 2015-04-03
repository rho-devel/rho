expected <- eval(parse(text="list(\"numeric_version\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(list(\"numeric_version\", \"numeric_version\"), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

