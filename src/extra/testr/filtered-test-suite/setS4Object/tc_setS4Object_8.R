expected <- eval(parse(text="structure(function (x, i, j, ...) x@aa[[i]], target = structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")), defined = structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(function (x, i, j, ...) x@aa[[i]], target = structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")), defined = structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\")), TRUE, 0L)"));     
.Internal(`setS4Object`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

