expected <- eval(parse(text="structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(character(0), .Names = character(0), package = character(0), class = structure(\"signature\", package = \"methods\")), value = structure(\"signature\", package = \"methods\"))"));   
do.call(`class<-`, argv);   
}, o=expected);   

