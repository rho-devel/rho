expected <- eval(parse(text="0L"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"), structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"), FALSE)"));             
.Internal(anyDuplicated(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

