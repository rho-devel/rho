expected <- eval(parse(text="NA_real_"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(0.104166666666667, 0.285714285714286, 0.285714285714286, NA))"));             
.Internal(mean(argv[[1]]));             
}, o=expected);             

