expected <- eval(parse(text="c(NA, 3L, 4L)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(NA, 3L, 4L), 3L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

