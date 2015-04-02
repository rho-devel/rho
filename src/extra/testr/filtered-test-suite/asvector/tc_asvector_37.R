expected <- eval(parse(text="structure(list(bg = \"lightblue\"), .Names = \"bg\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(\"lightblue\", .Names = \"bg\"), \"list\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

