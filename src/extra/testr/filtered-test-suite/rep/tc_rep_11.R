expected <- eval(parse(text="c(NA, NA, 30, -30)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(NA, NA, 30, -30), 4L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

