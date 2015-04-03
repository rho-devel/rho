expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), fac = structure(c(1L, 3L, 2L, 3L, 3L, 1L, 2L, 3L, 2L, 2L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = c(\"x\", \"y\", \"fac\"), row.names = c(NA, -10L), class = \"data.frame\"), structure(list(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), fac = structure(c(1L, 3L, 2L, 3L, 3L, 1L, 2L, 3L, 2L, 2L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = c(\"x\", \"y\", \"fac\"), row.names = c(NA, 10L), class = \"data.frame\"), TRUE, TRUE, TRUE, TRUE, FALSE)"));      
.Internal(`identical`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));      
}, o=expected);      

