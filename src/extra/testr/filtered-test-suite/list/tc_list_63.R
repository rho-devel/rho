expected <- eval(parse(text="structure(list(fit = structure(numeric(0), .Dim = c(10L, 0L), constant = 0), se.fit = structure(numeric(0), .Dim = c(10L, 0L)), df = 10L, residual.scale = 0.523484262069588), .Names = c(\"fit\", \"se.fit\", \"df\", \"residual.scale\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(fit = structure(numeric(0), .Dim = c(10L, 0L), constant = 0), se.fit = structure(numeric(0), .Dim = c(10L, 0L)), df = 10L, residual.scale = 0.523484262069588)"));         
do.call(`list`, argv);         
}, o=expected);         

