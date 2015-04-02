expected <- eval(parse(text="-Inf"));          
test(id=0, code={          
argv <- eval(parse(text="list(-Inf, 1L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

