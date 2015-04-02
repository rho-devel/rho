expected <- eval(parse(text="c(3L, 6L)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(3L, 6L), 2L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

