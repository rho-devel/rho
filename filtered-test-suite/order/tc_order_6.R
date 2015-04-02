expected <- eval(parse(text="1:6"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, c(25, 50, 100, 250, 500, 1e+05))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

