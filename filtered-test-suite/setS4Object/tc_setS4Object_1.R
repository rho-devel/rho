expected <- eval(parse(text="structure(\"ObjectsWithPackage\", class = structure(\"signature\", package = \"methods\"), .Names = \".Object\", package = \"methods\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(\"ObjectsWithPackage\", class = structure(\"signature\", package = \"methods\"), .Names = \".Object\", package = \"methods\"), TRUE, 0L)"));     
.Internal(`setS4Object`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

