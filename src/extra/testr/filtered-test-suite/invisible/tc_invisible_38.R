expected <- eval(parse(text="structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\"), c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = c(\"c0\", \"c0\"), row.names = integer(0), class = \"data.frame\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\"), c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = c(\"c0\", \"c0\"), row.names = integer(0), class = \"data.frame\"))"));             
do.call(`invisible`, argv);             
}, o=expected);             

