expected <- eval(parse(text="structure(list(), .Names = character(0))"));          
test(id=0, code={          
argv <- eval(parse(text="list(NULL)"));          
.Internal(options(argv[[1]]));          
}, o=expected);          

