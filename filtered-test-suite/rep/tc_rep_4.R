expected <- eval(parse(text="list()"));          
test(id=0, code={          
argv <- eval(parse(text="list(list(), 0L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

