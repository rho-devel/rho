expected <- eval(parse(text="structure(list(structure(integer(0), .Label = character(0), class = \"factor\")), row.names = character(0), class = \"data.frame\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(df0 = structure(list(structure(integer(0), .Label = character(0), class = \"factor\")), row.names = character(0), class = \"data.frame\")), .Names = \"df0\", row.names = \"c0\", class = \"data.frame\"), 1L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

