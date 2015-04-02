expected <- eval(parse(text="c(34, 35, 19, 37)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(x = c(0, 0, 1, 1), y = c(2, 2, 9, 9), z = c(0, 0, -3, -3), u = c(34, 35, 19, 37)), .Names = c(\"x\", \"y\", \"z\", \"u\"), row.names = c(2L, 90L, 25L, 50L), class = \"data.frame\"), 4L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

