expected <- eval(parse(text="c(2L, 3L, 1L)"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, c(NA, \"Ripley\", \"Venables & Smith\"))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

