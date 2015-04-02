expected <- eval(parse(text="structure(\"MethodDefinition\", package = \"methods\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(function (qr, y) .Call(sparseQR_resid_fitted, qr, y, TRUE), target = structure(c(\"sparseQR\", \"ddenseMatrix\"), .Names = c(\"qr\", \"y\"), package = c(\"Matrix\", \"Matrix\"), class = structure(\"signature\", package = \"methods\")), defined = structure(c(\"sparseQR\", \"ddenseMatrix\"), .Names = c(\"qr\", \"y\"), package = c(\"Matrix\", \"Matrix\"), class = structure(\"signature\", package = \"methods\")), generic = structure(\"qr.resid\", package = \"base\"), class = structure(\"MethodDefinition\", package = \"methods\")))"));        
do.call(`class`, argv);        
}, o=expected);        

