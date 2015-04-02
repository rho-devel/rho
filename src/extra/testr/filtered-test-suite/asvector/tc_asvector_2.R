expected <- eval(parse(text="character(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(character(0), package = character(0), class = structure(\"ObjectsWithPackage\", package = \"methods\")), \"character\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

