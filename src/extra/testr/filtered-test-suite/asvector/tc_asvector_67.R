expected <- eval(parse(text="\"1.6\""));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(1.6, class = \"object_size\"), \"character\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

