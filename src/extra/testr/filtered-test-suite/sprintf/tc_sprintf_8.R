expected <- eval(parse(text="\"plot_01\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"plot_%02g\", 1L)"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

