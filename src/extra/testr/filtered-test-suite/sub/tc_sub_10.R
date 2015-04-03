expected <- eval(parse(text="structure(\"MASS\", .Names = \"Suggests\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(\"^[[:space:]]*([[:alnum:].]+).*$\", \"\\\\1\", structure(\"MASS\", .Names = \"Suggests\"), FALSE, FALSE, FALSE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

