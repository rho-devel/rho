expected <- eval(parse(text="c(0L, 2L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(height = numeric(0), weight = numeric(0)), .Names = c(\"height\", \"weight\"), class = \"data.frame\", row.names = integer(0)))"));       
do.call(`dim`, argv);       
}, o=expected);       

