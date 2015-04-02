expected <- eval(parse(text="list(c(3L, 5L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(A = c(3L, 5L), B = c(3L, 5L), C = c(3L, 5L), D = c(3L, 5L)), .Names = c(\"A\", \"B\", \"C\", \"D\")), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

