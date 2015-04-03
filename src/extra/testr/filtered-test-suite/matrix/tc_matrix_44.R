expected <- eval(parse(text="structure(list(1:3, 4:6, 3.14159265358979, c(\"a\", \"b\", \"c\")), .Dim = c(2L, 2L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(a1 = 1:3, a2 = 4:6, a3 = 3.14159265358979, a4 = c(\"a\", \"b\", \"c\")), .Names = c(\"a1\", \"a2\", \"a3\", \"a4\")), 2, 2, FALSE, NULL, FALSE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

