expected <- eval(parse(text="structure(c(\"a\", \"2\", \"3.14159265358979+2i\"), .Names = c(\"a\", \"b\", \"c\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(a = \"a\", b = 2, c = 3.14159265358979+2i), .Names = c(\"a\", \"b\", \"c\")), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

