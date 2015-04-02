expected <- eval(parse(text="1:3"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, c(1L, 2L, NA))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

