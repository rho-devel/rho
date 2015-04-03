expected <- eval(parse(text="1:2"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, c(1L, 1L), c(5L, 5L))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));             
}, o=expected);             

