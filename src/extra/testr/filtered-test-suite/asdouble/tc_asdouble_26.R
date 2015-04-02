expected <- eval(parse(text="c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4)"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L), .Label = c(\"Rural Male\", \"Rural Female\", \"Urban Male\", \"Urban Female\"), class = \"factor\", .Dim = c(5L, 4L)))"));     
do.call(`as.double`, argv);     
}, o=expected);     

