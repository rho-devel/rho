expected <- eval(parse(text="structure(function (qr, complete = FALSE, Dvec) standardGeneric(\"qr.Q\"), generic = character(0), package = character(0), group = list(), valueClass = character(0), signature = character(0), default = quote(`\\001NULL\\001`), skeleton = quote(`<undef>`()), class = structure(\"standardGeneric\", package = \"methods\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(function (qr, complete = FALSE, Dvec) standardGeneric(\"qr.Q\"), generic = character(0), package = character(0), group = list(), valueClass = character(0), signature = character(0), default = quote(`\\001NULL\\001`), skeleton = quote(`<undef>`()), class = structure(\"standardGeneric\", package = \"methods\")), TRUE, 0L)"));     
.Internal(`setS4Object`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

