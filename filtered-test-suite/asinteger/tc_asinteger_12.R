expected <- eval(parse(text="c(1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0), .Dim = c(13L, 1L), .Dimnames = list(c(\"59\", \"115\", \"156\", \"268\", \"329\", \"431\", \"448\", \"477\", \"638\", \"803\", \"855\", \"1040\", \"1106\"), NULL)))"));              
do.call(`as.integer`, argv);              
}, o=expected);              

