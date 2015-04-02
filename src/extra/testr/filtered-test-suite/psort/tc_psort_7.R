expected <- eval(parse(text="c(1, 2, 2, 3, 3, 4, 4, 5)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1, 2, 2, 3, 3, 4, 4, 5), 1:8)"));      
.Internal(`psort`(argv[[1]], argv[[2]]));      
}, o=expected);      

