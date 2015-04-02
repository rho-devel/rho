expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"'x' is neither a vector nor a matrix: using as.numeric(x)\", quote(dotchart(table(infert$education))))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

