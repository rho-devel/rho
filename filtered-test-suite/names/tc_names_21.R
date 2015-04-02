expected <- eval(parse(text="c(\"Surv(time, status)\", \"x\")"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(`Surv(time, status)` = structure(c(9, 1, 1, 6, 6, 8, 1, 1, 0, 1, 1, 0), .Dim = c(6L, 2L), .Dimnames = list(NULL, c(\"time\", \"status\")), class = \"Surv\", type = \"right\"), x = c(0, 1, 1, 1, 0, 0)), .Names = c(\"Surv(time, status)\", \"x\"), class = \"data.frame\", row.names = c(1L, 3L, 4L, 5L, 6L, 7L)))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

