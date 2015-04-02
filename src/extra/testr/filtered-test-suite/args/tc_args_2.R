expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(character(0))"));   
.Internal(args(argv[[1]]));   
}, o=expected);   

