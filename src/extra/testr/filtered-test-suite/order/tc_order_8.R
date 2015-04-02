expected <- eval(parse(text="1:2"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, c(-1.90479340955971, 0.152878714793717))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

