expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"glm.fit: algorithm stopped at boundary value\", NULL)"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

