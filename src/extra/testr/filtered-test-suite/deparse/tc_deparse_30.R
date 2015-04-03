expected <- eval(parse(text="\"structure(FALSE, .Dim = 1L)\""));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(FALSE, .Dim = 1L), 60L, FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

