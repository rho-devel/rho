expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(function (e1, e2) standardGeneric(\"/\", .Primitive(\"/\")), generic = structure(\"/\", package = \"base\"), package = \"base\", group = list(\"Arith\"), valueClass = character(0), signature = c(\"e1\", \"e2\"), default = .Primitive(\"/\"), skeleton = quote(.Primitive(\"/\")(e1, e2)), class = structure(\"standardGeneric\", package = \"methods\")))"));       
do.call(`is.list`, argv);       
}, o=expected);       

