expected <- eval(parse(text="TRUE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(x = numeric(0), y = numeric(0), fac = structure(integer(0), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = c(\"x\", \"y\", \"fac\"), row.names = integer(0), class = \"data.frame\"), \"data.frame\", FALSE)"));         
.Internal(`inherits`(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

