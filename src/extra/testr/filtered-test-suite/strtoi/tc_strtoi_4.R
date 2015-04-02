expected <- eval(parse(text="NA_integer_"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"1.3\", 16L)"));     
.Internal(strtoi(argv[[1]], argv[[2]]));     
}, o=expected);     

