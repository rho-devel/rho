expected <- eval(parse(text="numeric(0)"));          
test(id=0, code={          
argv <- eval(parse(text="list(0, 0L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

