expected <- eval(parse(text="list(numeric(0), numeric(0))"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\"), structure(list(height = numeric(0), weight = numeric(0)), .Names = c(\"height\", \"weight\"), row.names = integer(0), class = \"data.frame\")), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

