expected <- eval(parse(text="list(structure(function (e1, e2) standardGeneric(\"Ops\"), generic = structure(\"Ops\", package = \"base\"), package = \"base\", group = list(), valueClass = character(0), signature = c(\"e1\", \"e2\"), default = quote(`\\001NULL\\001`), skeleton = quote((function (e1, e2) stop(\"invalid call in method dispatch to 'Ops' (no default method)\", domain = NA))(e1, e2)), groupMembers = list(\"Arith\", \"Compare\", \"Logic\"), class = structure(\"groupGenericFunction\", package = \"methods\")))"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(list(structure(function (e1, e2) standardGeneric(\"Ops\"), generic = structure(\"Ops\", package = \"base\"), package = \"base\", group = list(), valueClass = character(0), signature = c(\"e1\", \"e2\"), default = quote(`\\001NULL\\001`), skeleton = quote((function (e1, e2) stop(\"invalid call in method dispatch to 'Ops' (no default method)\", domain = NA))(e1, e2)), groupMembers = list(\"Arith\", \"Compare\", \"Logic\"), class = structure(\"groupGenericFunction\", package = \"methods\")))), FALSE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

