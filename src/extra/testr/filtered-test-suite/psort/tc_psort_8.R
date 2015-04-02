expected <- eval(parse(text="27:28"));      
test(id=0, code={      
argv <- eval(parse(text="list(27:28, 1:2)"));      
.Internal(`psort`(argv[[1]], argv[[2]]));      
}, o=expected);      

