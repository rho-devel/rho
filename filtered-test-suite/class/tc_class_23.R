expected <- eval(parse(text="structure(\"standardGeneric\", package = \"methods\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(function (a, b, ...) standardGeneric(\"solve\"), generic = structure(\"solve\", package = \"base\"), package = \"base\", group = list(), valueClass = character(0), signature = c(\"a\", \"b\"), default = structure(function (a, b, ...) UseMethod(\"solve\"), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"a\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"a\", package = \"methods\"), generic = structure(\"solve\", package = \"base\"), class = structure(\"derivedDefaultMethod\", package = \"methods\")), skeleton = quote((function (a, b, ...) UseMethod(\"solve\"))(a, b, ...)), class = structure(\"standardGeneric\", package = \"methods\")))"));        
do.call(`class`, argv);        
}, o=expected);        

