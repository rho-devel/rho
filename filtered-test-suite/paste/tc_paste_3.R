expected <- eval(parse(text="character(0)"));                
test(id=0, code={                
argv <- eval(parse(text="list(list(), \" \", NULL)"));                
.Internal(paste(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

