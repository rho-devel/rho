expected <- eval(parse(text="\"character(0)\""));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(c0 = structure(character(0), class = \"AsIs\")), .Names = \"c0\", row.names = character(0), class = \"data.frame\"), \"character\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

