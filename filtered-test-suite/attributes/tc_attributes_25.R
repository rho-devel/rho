expected <- eval(parse(text="structure(list(package = character(0), class = structure(\"ObjectsWithPackage\", package = \"methods\")), .Names = c(\"package\", \"class\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(character(0), package = character(0), class = structure(\"ObjectsWithPackage\", package = \"methods\")))"));       
do.call(`attributes`, argv);       
}, o=expected);       

