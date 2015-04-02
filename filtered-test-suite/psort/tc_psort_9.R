expected <- eval(parse(text="c(-Inf, -Inf, Inf, Inf)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(-Inf, -Inf, Inf, Inf), 1:4)"));      
.Internal(`psort`(argv[[1]], argv[[2]]));      
}, o=expected);      

