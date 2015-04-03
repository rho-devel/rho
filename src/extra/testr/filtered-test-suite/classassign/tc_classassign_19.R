expected <- eval(parse(text="structure(function (obj, force = FALSE) standardGeneric(\"unname\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"obj\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"obj\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(function (obj, force = FALSE) standardGeneric(\"unname\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"obj\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"obj\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\")), value = structure(\"MethodDefinition\", package = \"methods\"))"));   
do.call(`class<-`, argv);   
}, o=expected);   

