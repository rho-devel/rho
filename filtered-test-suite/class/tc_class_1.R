expected <- eval(parse(text="structure(\"standardGeneric\", package = \"methods\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(function (x) standardGeneric(\"exp\", .Primitive(\"exp\")), generic = structure(\"exp\", package = \"base\"), package = \"base\", group = list(\"Math\"), valueClass = character(0), signature = \"x\", default = .Primitive(\"exp\"), skeleton = quote(.Primitive(\"exp\")(x)), class = structure(\"standardGeneric\", package = \"methods\")))"));        
do.call(`class`, argv);        
}, o=expected);        

