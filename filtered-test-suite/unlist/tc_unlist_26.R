expected <- eval(parse(text="structure(list(a1 = 1:5, a2 = c(\"A\", \"B\", \"C\", \"D\", \"E\"), b = \"Z\", c = NA), .Names = c(\"a1\", \"a2\", \"b\", \"c\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(a = list(1:5, c(\"A\", \"B\", \"C\", \"D\", \"E\")), b = \"Z\", c = NA), .Names = c(\"a\", \"b\", \"c\")), FALSE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

