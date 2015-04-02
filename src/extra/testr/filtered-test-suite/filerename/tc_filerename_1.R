expected <- eval(parse(text="logical(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(character(0), character(0))"));   
.Internal(file.rename(argv[[1]], argv[[2]]));   
}, o=expected);   

