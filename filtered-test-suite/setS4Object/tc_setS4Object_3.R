expected <- eval(parse(text="structure(character(0), package = character(0), class = structure(\"ObjectsWithPackage\", package = \"methods\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(character(0), package = character(0), class = structure(\"ObjectsWithPackage\", package = \"methods\")), TRUE, 0L)"));     
.Internal(`setS4Object`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

