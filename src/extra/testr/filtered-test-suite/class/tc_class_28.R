expected <- eval(parse(text="structure(\"MethodDefinition\", package = \"methods\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(function (x, uplo) {    if (uplo == x@uplo) x else t(x)}, target = structure(c(\"nsCMatrix\", \"character\"), .Names = c(\"x\", \"uplo\"), package = c(\"Matrix\", \"methods\"), class = structure(\"signature\", package = \"methods\")), defined = structure(c(\"nsCMatrix\", \"character\"), .Names = c(\"x\", \"uplo\"), package = c(\"Matrix\", \"methods\"), class = structure(\"signature\", package = \"methods\")), generic = structure(\"forceSymmetric\", package = \"Matrix\"), class = structure(\"MethodDefinition\", package = \"methods\")))"));        
do.call(`class`, argv);        
}, o=expected);        

