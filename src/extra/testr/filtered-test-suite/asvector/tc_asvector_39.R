expected <- eval(parse(text="structure(list(`character(0)` = structure(integer(0), .Label = character(0), class = \"factor\")), row.names = character(0), class = \"data.frame\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(`character(0)` = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"character(0)\", row.names = character(0), class = \"data.frame\"), \"pairlist\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

