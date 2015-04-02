expected <- eval(parse(text="structure(c(0, 0.00470181932247022, -2.53999454870812, -15.9270296583589), .Dim = c(2L, 2L))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(-0.409148064492827, 0, 0.486127240746069, 0.000757379686646223), .Dim = c(2L, 2L), .Dimnames = list(c(\"Vm\", \"K\"), NULL)), structure(c(0, 6.20800822278518, 6.20800822278518, -25013.7571686415), .Dim = c(2L, 2L)))"));         
.Internal(crossprod(argv[[1]], argv[[2]]));         
}, o=expected);         

