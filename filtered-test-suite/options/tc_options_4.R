expected <- eval(parse(text="structure(list(ts.eps = 1e-05), .Names = \"ts.eps\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"ts.eps\")"));   
.Internal(`options`(argv[[1]]));   
}, o=expected);   

