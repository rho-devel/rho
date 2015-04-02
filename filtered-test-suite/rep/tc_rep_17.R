expected <- eval(parse(text="c(1, 1)"));          
test(id=0, code={          
argv <- eval(parse(text="list(1, 2L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

