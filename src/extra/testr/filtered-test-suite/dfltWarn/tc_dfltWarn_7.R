expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"'drop' argument will be ignored\", quote(`[.data.frame`(women, \"height\", drop = FALSE)))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

