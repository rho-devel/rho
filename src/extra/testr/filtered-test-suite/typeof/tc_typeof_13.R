expected <- eval(parse(text="\"integer\""));                
test(id=0, code={                
argv <- eval(parse(text="list(c(2L, 1L, NA))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

