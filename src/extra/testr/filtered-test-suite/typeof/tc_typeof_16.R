expected <- eval(parse(text="\"double\""));                
test(id=0, code={                
argv <- eval(parse(text="list(1e+05)"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

