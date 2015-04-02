expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"NaNs produced\", quote(log(ifelse(y == 0, 1, y/mu))))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

