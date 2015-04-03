expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(FALSE, 1L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

