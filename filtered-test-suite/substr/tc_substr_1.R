expected <- eval(parse(text="\"we\""));    
test(id=0, code={    
argv <- eval(parse(text="list(\"weight\", 1L, 2L)"));    
.Internal(`substr`(argv[[1]], argv[[2]], argv[[3]]));    
}, o=expected);    

