expected <- eval(parse(text="character(0)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"%o\", integer(0))"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

