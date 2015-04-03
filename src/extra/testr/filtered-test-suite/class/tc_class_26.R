expected <- eval(parse(text="structure(\"MethodDefinition\", package = \"methods\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(function (x, type, ...) .Call(dgeMatrix_norm, as(x, \"dgeMatrix\"), type), target = structure(c(\"matrix\", \"character\"), .Names = c(\"x\", \"type\"), package = c(\"methods\", \"methods\"), class = structure(\"signature\", package = \"methods\")), defined = structure(c(\"matrix\", \"character\"), .Names = c(\"x\", \"type\"), package = c(\"methods\", \"methods\"), class = structure(\"signature\", package = \"methods\")), generic = structure(\"norm\", package = \"base\"), class = structure(\"MethodDefinition\", package = \"methods\")))"));        
do.call(`class`, argv);        
}, o=expected);        

