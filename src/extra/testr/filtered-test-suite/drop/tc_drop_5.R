expected <- eval(parse(text="1"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1, .Dim = c(1L, 1L)))"));     
.Internal(`drop`(argv[[1]]));     
}, o=expected);     

