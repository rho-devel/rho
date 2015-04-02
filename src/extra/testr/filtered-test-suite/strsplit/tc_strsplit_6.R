expected <- eval(parse(text="structure(list(Title = \"Formal Methods and Classes\"), .Names = \"Title\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(\"Formal Methods and Classes\", .Names = \"Title\"), \"\\n\\n\", TRUE, FALSE, TRUE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

