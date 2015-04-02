expected <- eval(parse(text="\" \""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"%*s\", 1, \"\")"));           
.Internal(sprintf(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

