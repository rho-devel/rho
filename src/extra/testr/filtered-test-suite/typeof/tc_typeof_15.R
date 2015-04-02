expected <- eval(parse(text="\"complex\""));                
test(id=0, code={                
argv <- eval(parse(text="list(c(1.1+0i, NA, 3+0i))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

