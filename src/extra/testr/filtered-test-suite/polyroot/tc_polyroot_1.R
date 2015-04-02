expected <- eval(parse(text="-0.5+0i"));      
test(id=0, code={      
argv <- eval(parse(text="list(1:2)"));      
.Internal(polyroot(argv[[1]]));      
}, o=expected);      

