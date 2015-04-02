expected <- eval(parse(text="structure(1:4, .Names = c(\"a\", \"b\", \"c\", \"d\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1:4, .Dim = c(4L, 1L), .Dimnames = list(c(\"a\", \"b\", \"c\", \"d\"), NULL)))"));     
.Internal(`drop`(argv[[1]]));     
}, o=expected);     

