expected <- eval(parse(text="\"\""));                
test(id=0, code={                
argv <- eval(parse(text="list(\"\", 1L, 2L)"));                
.Internal(substr(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

