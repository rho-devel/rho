expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(x = 1L), .Names = \"x\", row.names = c(NA, -1L), class = \"data.frame\"), structure(list(x = 1L), .Names = \"x\", row.names = c(NA, -1L), class = \"data.frame\"), TRUE, TRUE, TRUE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

