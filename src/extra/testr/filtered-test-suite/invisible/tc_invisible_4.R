expected <- eval(parse(text="structure(list(height = numeric(0), weight = numeric(0)), .Names = c(\"height\", \"weight\"), row.names = integer(0), class = \"data.frame\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(height = numeric(0), weight = numeric(0)), .Names = c(\"height\", \"weight\"), row.names = integer(0), class = \"data.frame\"))"));      
do.call(`invisible`, argv);      
}, o=expected);      

