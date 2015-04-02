expected <- eval(parse(text="\"FALSE\""));               
test(id=0, code={               
argv <- eval(parse(text="list(FALSE, \"character\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

