expected <- eval(parse(text="structure(list(`0` = structure(47.432, .Names = \"(Intercept)\"), `1` = structure(12.482, .Names = \"group2\")), .Names = c(\"0\", \"1\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(47.432, 12.482), .Names = c(\"(Intercept)\", \"group2\")), structure(1:2, .Label = c(\"0\", \"1\"), class = \"factor\"))"));             
.Internal(split(argv[[1]], argv[[2]]));             
}, o=expected);             

