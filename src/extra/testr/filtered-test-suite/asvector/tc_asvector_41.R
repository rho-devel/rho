expected <- eval(parse(text="list(\"GRID.VP.12\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(list(\"GRID.VP.12\"), \"list\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

