expected <- eval(parse(text="c(255L, 63L, 123L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"0xff\", \"077\", \"123\"), 0L)"));     
.Internal(strtoi(argv[[1]], argv[[2]]));     
}, o=expected);     

