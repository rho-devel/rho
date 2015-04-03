expected <- eval(parse(text="structure(1:120, .Dim = 2:5, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"), NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\")))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:120, .Dim = 2:5, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"), NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\"))), 1:4, TRUE)"));   
.Internal(`aperm`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

