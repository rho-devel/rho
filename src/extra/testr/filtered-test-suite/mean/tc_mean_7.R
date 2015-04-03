expected <- eval(parse(text="1.47130567537631e-314"));             
test(id=0, code={             
argv <- eval(parse(text="list(1.47130567537631e-314)"));             
.Internal(mean(argv[[1]]));             
}, o=expected);             

