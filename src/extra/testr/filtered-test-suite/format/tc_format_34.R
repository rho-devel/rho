expected <- eval(parse(text="structure(c(\"213198964\", \"   652425\"), .Names = c(\"null.deviance\", \"deviance\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(213198964, 652424.52183908), .Names = c(\"null.deviance\", \"deviance\")), FALSE, 5L, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

