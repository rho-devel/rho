expected <- eval(parse(text="list(\"1\", \"2\", \"3\", \"4\", \"5\", \"1\", \"2\", \"3\", \"4\", \"5\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"1\", \"2\", \"3\", \"4\", \"5\"), .Dim = 10L), \".\", TRUE, FALSE, FALSE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

