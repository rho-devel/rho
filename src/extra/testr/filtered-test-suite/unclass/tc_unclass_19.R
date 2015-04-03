expected <- eval(parse(text="structure(list(b = structure(c(3L, 1L, 2L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\"), a = structure(c(1386423981.90268, 1386403981.90268, 1386413981.90268), class = c(\"POSIXct\", \"POSIXt\"))), .Names = c(\"b\", \"a\"), row.names = c(3L, 1L, 2L))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(b = structure(c(3L, 1L, 2L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\"), a = structure(c(1386423981.90268, 1386403981.90268, 1386413981.90268), class = c(\"POSIXct\", \"POSIXt\"))), .Names = c(\"b\", \"a\"), row.names = c(3L, 1L, 2L)))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

