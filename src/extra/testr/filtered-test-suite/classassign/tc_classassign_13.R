expected <- eval(parse(text="structure(function (qr, y, k = qr$rank) standardGeneric(\"qr.fitted\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"qr\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"qr\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(function (qr, y, k = qr$rank) standardGeneric(\"qr.fitted\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"qr\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"qr\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\")), value = structure(\"MethodDefinition\", package = \"methods\"))"));   
do.call(`class<-`, argv);   
}, o=expected);   

