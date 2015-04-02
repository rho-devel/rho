expected <- eval(parse(text="structure(list(names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")), .Names = c(\"names\", \"package\", \"class\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")))"));       
do.call(`attributes`, argv);       
}, o=expected);       

