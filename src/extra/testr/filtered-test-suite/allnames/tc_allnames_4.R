expected <- eval(parse(text="character(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"), structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"), -1L, FALSE)"));   
.Internal(all.names(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));   
}, o=expected);   

