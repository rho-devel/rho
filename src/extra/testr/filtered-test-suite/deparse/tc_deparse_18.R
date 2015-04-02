expected <- eval(parse(text="\"Inf\""));          
test(id=0, code={          
argv <- eval(parse(text="list(Inf, 60L, FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

