expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"1 y value <= 0 omitted from logarithmic plot\", quote(xy.coords(x, NULL, log = log)))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

