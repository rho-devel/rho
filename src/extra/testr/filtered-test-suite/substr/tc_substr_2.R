expected <- eval(parse(text="c(\"    \", \"\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(\"        \", \"        \"), 1L, c(4L, -16L))"));    
.Internal(`substr`(argv[[1]], argv[[2]], argv[[3]]));    
}, o=expected);    

