expected <- eval(parse(text="structure(1L, .Names = \"V1\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(TRUE, .Names = \"V1\"))"));   
.Internal(`which`(argv[[1]]));   
}, o=expected);   

