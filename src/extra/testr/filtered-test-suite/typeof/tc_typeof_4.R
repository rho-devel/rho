expected <- eval(parse(text="\"closure\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(function (x, y = NULL) standardGeneric(\"tcrossprod\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), generic = character(0), class = structure(\"MethodDefinition\", package = \"methods\")))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

