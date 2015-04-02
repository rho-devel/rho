expected <- eval(parse(text="7:8"));      
test(id=0, code={      
argv <- eval(parse(text="list(7:8, 1:2)"));      
.Internal(`psort`(argv[[1]], argv[[2]]));      
}, o=expected);      

