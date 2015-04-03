expected <- eval(parse(text="\"flld.c\""));     
test(id=0, code={     
argv <- eval(parse(text="list(\"filled.contour\", 6, TRUE)"));     
.Internal(abbreviate(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

