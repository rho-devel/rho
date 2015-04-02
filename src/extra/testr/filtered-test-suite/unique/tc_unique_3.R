expected <- eval(parse(text="c(1, 2, 4, 6, 8, 9, 11, 13, 14, 16, 3, 5, 7, 10, 12, 15, 17)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1, 2, 4, 6, 8, 9, 11, 13, 14, 16, 3, 5, 7, 9, 10, 12, 14, 15, 17, 17), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

