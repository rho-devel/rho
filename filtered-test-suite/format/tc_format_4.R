expected <- eval(parse(text="structure(c(\"axx\", \"  b\", \"  c\", \"  d\", \"  e\", \"  f\", \"  g\", \"  h\"), .Dim = c(2L, 4L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(\"axx\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\"), .Dim = c(2L, 4L)), FALSE, NULL, 0L, NULL, 1L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

