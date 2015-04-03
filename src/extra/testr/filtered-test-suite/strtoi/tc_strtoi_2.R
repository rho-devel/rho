expected <- eval(parse(text="448L"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"700\", 8L)"));     
.Internal(strtoi(argv[[1]], argv[[2]]));     
}, o=expected);     

