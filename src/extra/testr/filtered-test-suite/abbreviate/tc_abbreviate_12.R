expected <- eval(parse(text="\"dtrMt-\""));     
test(id=0, code={     
argv <- eval(parse(text="list(\"dtrMatrix-class\", 6, TRUE)"));     
.Internal(abbreviate(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

