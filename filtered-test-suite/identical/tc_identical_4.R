expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(3.14159265358979, comment = \"Start with pi\", class = structure(\"num1\", package = \".GlobalEnv\")), structure(3.14159265358979, comment = \"Start with pi\", class = structure(\"num1\", package = \".GlobalEnv\")), TRUE, TRUE, TRUE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

