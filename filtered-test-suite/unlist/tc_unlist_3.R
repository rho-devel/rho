expected <- eval(parse(text="structure(\"A\", .Names = \"x\", package = \".GlobalEnv\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(\"A\", .Names = \"x\", package = \".GlobalEnv\"), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

