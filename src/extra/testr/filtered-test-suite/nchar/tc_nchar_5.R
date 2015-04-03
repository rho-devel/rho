expected <- eval(parse(text="c(4L, 4L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"Var1\", \"Var2\"), \"bytes\", FALSE)"));                
.Internal(nchar(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

