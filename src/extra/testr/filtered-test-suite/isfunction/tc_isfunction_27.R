expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(function (x) standardGeneric(\"dim\", .Primitive(\"dim\")), generic = structure(\"dim\", package = \"base\"), package = \"base\", group = list(), valueClass = character(0), signature = \"x\", default = .Primitive(\"dim\"), skeleton = quote(.Primitive(\"dim\")(x)), class = structure(\"standardGeneric\", package = \"methods\")))"));      
do.call(`is.function`, argv);      
}, o=expected);      

