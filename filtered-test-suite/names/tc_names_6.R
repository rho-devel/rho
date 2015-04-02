expected <- eval(parse(text="\"groups\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(groups = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c(\"1\", \"2\", \"3\"), class = \"factor\")), .Names = \"groups\"))"));         
do.call(`names`, argv);         
}, o=expected);         

