expected <- eval(parse(text="NA"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

