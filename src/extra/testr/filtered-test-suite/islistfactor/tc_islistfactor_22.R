expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(a = \"a\", b = 2, c = 3.14159265358979+2i), .Names = c(\"a\", \"b\", \"c\")), TRUE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

