expected <- eval(parse(text="complex(0)"));      
test(id=0, code={      
argv <- eval(parse(text="list(FALSE)"));      
.Internal(polyroot(argv[[1]]));      
}, o=expected);      

