expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(function (x) standardGeneric(\"cosh\", .Primitive(\"cosh\")), generic = structure(\"cosh\", package = \"base\"), package = \"base\", group = list(\"Math\"), valueClass = character(0), signature = \"x\", default = .Primitive(\"cosh\"), skeleton = quote(.Primitive(\"cosh\")(x)), class = structure(\"standardGeneric\", package = \"methods\")), FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)"));      
.Internal(`identical`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));      
}, o=expected);      

