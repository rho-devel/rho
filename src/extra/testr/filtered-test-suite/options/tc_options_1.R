expected <- eval(parse(text="structure(list(survfit.print.n = NULL), .Names = \"survfit.print.n\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"survfit.print.n\")"));   
.Internal(`options`(argv[[1]]));   
}, o=expected);   

