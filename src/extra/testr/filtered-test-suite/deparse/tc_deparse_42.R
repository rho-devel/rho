expected <- eval(parse(text="\"c(FALSE, FALSE)\""));          
test(id=0, code={          
argv <- eval(parse(text="list(c(FALSE, FALSE), 60L, FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

