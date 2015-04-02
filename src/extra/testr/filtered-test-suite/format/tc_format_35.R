expected <- eval(parse(text="structure(character(0), .Dim = c(1L, 0L))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(integer(0), .Dim = c(1L, 0L), row.vars = structure(list(), .Names = character(0)), col.vars = structure(list(df0 = NULL), .Names = \"df0\")), FALSE, 7L, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

