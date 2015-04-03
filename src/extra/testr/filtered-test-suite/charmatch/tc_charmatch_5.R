expected <- eval(parse(text="1:2"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"0\", \"1\"), c(\"0\", \"1\"), 0)"));     
.Internal(charmatch(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

