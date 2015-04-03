expected <- eval(parse(text="\"GRID.text.6\""));                
test(id=0, code={                
argv <- eval(parse(text="list(list(\"GRID\", \"text\", \"6\"), \".\", NULL)"));                
.Internal(paste(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

