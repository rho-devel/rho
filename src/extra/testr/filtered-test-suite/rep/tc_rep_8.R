expected <- eval(parse(text="list(c(\"                  \", \"                \"))"));          
test(id=0, code={          
argv <- eval(parse(text="list(list(c(\"                  \", \"                \")), 1L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

