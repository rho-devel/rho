expected <- eval(parse(text="structure(c(\" 9.4\", \"10.2\", \" 9.2\", \" 4.4\", \" 3.5\", \" 2.7\"), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"Estimate\", \"Std.Err\")))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(9.4, 10.2, 9.2, 4.4, 3.5, 2.7), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"Estimate\", \"Std.Err\"))), FALSE, 2, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

