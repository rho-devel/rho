expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L), .Label = c(\"1\", \"2\"), class = \"factor\"), structure(list(f = structure(c(1L, 1L, 1L), .Label = c(\"1\", \"2\"), class = \"factor\"), u = structure(12:14, unit = \"kg\", class = \"avector\")), .Names = c(\"f\", \"u\"), row.names = 2:4, class = \"data.frame\"), TRUE, TRUE, TRUE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

