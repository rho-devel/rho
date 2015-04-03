expected <- eval(parse(text="\"logical(0)\""));          
test(id=0, code={          
argv <- eval(parse(text="list(logical(0), logical(0), FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

