expected <- eval(parse(text="integer(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(height = numeric(0), weight = numeric(0)), .Names = c(\"height\", \"weight\"), row.names = integer(0), class = \"data.frame\"), 0L)"));       
.Internal(`shortRowNames`(argv[[1]], argv[[2]]));       
}, o=expected);       

