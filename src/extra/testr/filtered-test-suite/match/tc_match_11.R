expected <- eval(parse(text="NA_integer_"));                
test(id=0, code={                
argv <- eval(parse(text="list(\"g\", \"l\", NA_character_, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

