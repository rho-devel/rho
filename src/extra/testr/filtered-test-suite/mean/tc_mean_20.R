expected <- eval(parse(text="NaN"));             
test(id=0, code={             
argv <- eval(parse(text="list(numeric(0))"));             
.Internal(mean(argv[[1]]));             
}, o=expected);             

