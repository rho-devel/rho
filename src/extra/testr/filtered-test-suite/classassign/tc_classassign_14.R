expected <- eval(parse(text="structure(function (x = 1, nrow, ncol) standardGeneric(\"diag\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(function (x = 1, nrow, ncol) standardGeneric(\"diag\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\")), value = structure(\"MethodDefinition\", package = \"methods\"))"));   
do.call(`class<-`, argv);   
}, o=expected);   

