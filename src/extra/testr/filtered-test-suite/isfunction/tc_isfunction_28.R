expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(function (x, na.rm = FALSE, dims = 1, ...) standardGeneric(\"rowMeans\"), generic = structure(\"rowMeans\", package = \"base\"), package = \"base\", group = list(), valueClass = character(0), signature = c(\"x\", \"na.rm\", \"dims\"), default = structure(function (x, na.rm = FALSE, dims = 1, ...) base::rowMeans(x, na.rm = na.rm, dims = dims, ...), target = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), defined = structure(\"ANY\", class = structure(\"signature\", package = \"methods\"), .Names = \"x\", package = \"methods\"), generic = structure(\"rowMeans\", package = \"base\"), class = structure(\"derivedDefaultMethod\", package = \"methods\")), skeleton = quote((function (x, na.rm = FALSE, dims = 1, ...) base::rowMeans(x, na.rm = na.rm, dims = dims, ...))(x, na.rm, dims, ...)), class = structure(\"standardGeneric\", package = \"methods\")))"));      
do.call(`is.function`, argv);      
}, o=expected);      

