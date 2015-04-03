expected <- eval(parse(text="c(1L, 1L, 1L, 1L, 1L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"0.5\", \"0.5\", \"0.5\", \"0.5\", \"0.5\"), 0.5, NA_integer_, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

