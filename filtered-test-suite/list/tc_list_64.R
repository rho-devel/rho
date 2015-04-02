expected <- eval(parse(text="structure(list(ANY = structure(function (x, y = NULL) .Internal(crossprod(x, y)), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), generic = structure(\"crossprod\", package = \"base\"), class = structure(\"derivedDefaultMethod\", package = \"methods\"))), .Names = \"ANY\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(ANY = structure(function (x, y = NULL) .Internal(crossprod(x, y)), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), generic = structure(\"crossprod\", package = \"base\"), class = structure(\"derivedDefaultMethod\", package = \"methods\")))"));         
do.call(`list`, argv);         
}, o=expected);         

