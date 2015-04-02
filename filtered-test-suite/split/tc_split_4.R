expected <- eval(parse(text="structure(list(`1` = c(0, 0, 0, 0, 0, 0, 1.48219693752374e-323, 0, 0, 0, 0, 0)), .Names = \"1\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 0, 0, 1.48219693752374e-323, 0, 0, 0, 0, 0), .Dim = c(1L, 12L), .Dimnames = list(NULL, c(\"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\"))), structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = \"1\", class = \"factor\"))"));             
.Internal(split(argv[[1]], argv[[2]]));             
}, o=expected);             

