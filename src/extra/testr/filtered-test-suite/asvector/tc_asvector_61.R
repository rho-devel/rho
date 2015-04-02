expected <- eval(parse(text="3.18309886183776e-301"));               
test(id=0, code={               
argv <- eval(parse(text="list(3.18309886183776e-301, \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

