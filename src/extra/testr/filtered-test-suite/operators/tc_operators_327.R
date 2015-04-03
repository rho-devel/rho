expected <- eval(parse(text="structure(list(c0 = logical(0)), .Names = \"c0\", row.names = integer(0), class = \"data.frame\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"), structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"))"));          
do.call(`%/%`, argv);          
}, o=expected);          

