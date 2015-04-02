expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(\"BunchKaufman\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), structure(\"Matrix\", .Names = \"x\", package = \"Matrix\", class = structure(\"signature\", package = \"methods\")), TRUE, TRUE, TRUE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

