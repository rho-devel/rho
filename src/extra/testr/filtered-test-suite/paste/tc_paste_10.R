expected <- eval(parse(text="c(\"( 1.124,\", \"( 1.056,\", \"( 1.059,\", \"( 0.932,\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(list(\"(\", structure(c(\" 1.124\", \" 1.056\", \" 1.059\", \" 0.932\"), .Dim = c(2L, 2L)), \",\"), \"\", NULL)"));                
.Internal(paste(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

