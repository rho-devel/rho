expected <- eval(parse(text="structure(list(a = c(1L, 4L, 7L), b = c(2L, 5L, 8L), c = c(3L, 6L, 9L)), .Names = c(\"a\", \"b\", \"c\"), class = \"data.frame\", row.names = c(NA, -3L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(a = c(1L, 4L, 7L), b = c(2L, 5L, 8L), c = c(3L, 6L, 9L)), .Names = c(\"a\", \"b\", \"c\"), class = \"data.frame\", row.names = c(NA, -3L)))"));             
do.call(`invisible`, argv);             
}, o=expected);             

