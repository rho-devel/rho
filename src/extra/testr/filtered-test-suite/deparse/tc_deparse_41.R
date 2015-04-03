expected <- eval(parse(text="\"FALSE\""));          
test(id=0, code={          
argv <- eval(parse(text="list(FALSE, 50L, FALSE, 69, 2L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

